{-
Bustle.Renderer: render nice Cairo diagrams from a list of D-Bus messages
Copyright (C) 2008 Collabora Ltd.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bustle.Renderer
    ( process
    )
where

import Prelude hiding (log)

import Bustle.Types
import Bustle.Diagram

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Ratio

import Control.Applicative ((<$>))
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad (forM_)

import Data.List (isPrefixOf, stripPrefix, sortBy)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ord (comparing)

process :: [Message] -> Either String [Shape]
process log = execRenderer (mapM_ munge log') (initialState initTime)

  where -- FIXME: really? Maybe we should allow people to be interested in,
        --        say, binding to signals?
        notDaemon m = (path . member) m /= "/org/freedesktop/DBus"
                   && sender m /= O (OtherName "<none>")

        relevant m@(Signal {}) = notDaemon m
        relevant m@(MethodCall {}) = notDaemon m
        relevant _ = True

        log' = filter relevant log

        initTime = case dropWhile (== 0) (map timestamp log') of
            t:_ -> t
            _   -> 0

newtype Renderer a = Renderer (WriterT [Shape]
                                (StateT RendererState
                                 (ErrorT String Identity)
                                ) a)
  deriving (Functor, Monad, MonadState RendererState, MonadWriter [Shape],
      MonadError String)

execRenderer :: Renderer () -> RendererState -> Either String [Shape]
execRenderer (Renderer act) = runIdentity . runErrorT . evalStateT (execWriterT act)

data RendererState =
    RendererState { apps :: Applications
                  , nextColumn :: Double
                  , pending :: Map Message (Double, Double)
                  , row :: Double
                  , mostRecentLabels :: Double
                  , startTime :: Milliseconds
                  }

initialState :: Milliseconds -> RendererState
initialState t = RendererState
    { apps = Map.empty
    , nextColumn = 470 -- FIXME: magic number :'(
    , pending = Map.empty
    , row = 0
    , mostRecentLabels = 0
    , startTime = t
    }

-- Maps unique connection name to the column representing that name, if
-- allocated, and a set of non-unique names for the connection, if any.
type Applications = Map UniqueName (Maybe Double, Set OtherName)

-- Finds a BusName in a map of applications
lookupApp :: BusName
          -> Applications
          -> Maybe (UniqueName, (Maybe Double, Set OtherName))
lookupApp n as = case n of
    U u -> (,) u <$> Map.lookup u as
    O o -> case filter (Set.member o . snd . snd) (Map.assocs as) of
              []         -> Nothing
              [details]  -> Just details
              several    -> error $ concat [ "internal error: "
                                           , show o
                                           , " in several apps: "
                                           , show several
                                           ]

-- Finds a BusName in the current state, yielding its column if it exists.  If
-- it exists, but previously lacked a column, a column is allocated.
getApp :: BusName -> Renderer (Maybe Double)
getApp n = do
    app <- gets (lookupApp n . apps)
    case app of
        Nothing -> return Nothing
        Just (u, details) -> Just <$> case details of
            (Just col, _) -> return col
            (Nothing, os) -> assignColumn u os
  where assignColumn :: UniqueName -> Set OtherName -> Renderer Double
        assignColumn u os = do
            x <- gets nextColumn
            modify $ \bs -> bs { nextColumn = x + 70 }
            modifyApps $ Map.insert u (Just x, os)

            -- FIXME: Does this really live here?
            currentRow <- gets row
            shape $ Header (bestNames u os) x (currentRow - 20)
            shape $ ClientLine x (currentRow - 5) (currentRow + 15)

            return x

-- Modify the application table directly.
modifyApps :: (Applications -> Applications) -> Renderer ()
modifyApps f = modify $ \bs -> bs { apps = f (apps bs) }

-- Updates the current set of applications in response to a well-known name's
-- owner changing.
updateApps :: OtherName -- name whose owner has changed.
           -> Change -- details of the change
           -> Renderer (Maybe Double, Maybe Double) -- the old and new owners' columns
updateApps n c = case c of
    Claimed new -> (,) Nothing `fmap` addOther n new
    Stolen old new -> (,) `fmap` remOther n old `ap` addOther n new
    Released old -> flip (,) Nothing `fmap` remOther n old

-- updateApps but ignore the reply.
updateApps_ :: OtherName -- name whose owner has changed.
            -> Change -- details of the change
            -> Renderer ()
updateApps_ n c = updateApps n c >> return ()

-- Adds a new unique name
addUnique :: UniqueName -> Renderer ()
addUnique n = modifyApps $ Map.insert n (Nothing, Set.empty)
  -- FIXME: this could trample on names that erroneously already exist...

-- Removes a unique name, yielding its column (if any)
remUnique :: UniqueName -> Renderer (Maybe Double)
remUnique n = do
    coord <- gets (fmap fst . Map.lookup n . apps)
    case coord of
      Just x  -> modifyApps (Map.delete n) >> return x
      Nothing -> throwError $ concat [ "corrupt log: "
                                     , show n
                                     , " apparently disconnected without connecting"
                                     ]

addOther, remOther :: OtherName -> UniqueName -> Renderer (Maybe Double)
-- Add a new well-known name to a unique name.
addOther n u = do
    a <- gets (Map.lookup u . apps)
    case a of
        Nothing -> throwError $ concat [ "corrupt log: "
                                       , show n
                                       , " claimed by unknown unique name "
                                       , show u
                                       ]
        Just (x, ns) -> do modifyApps (Map.insert u (x, Set.insert n ns))
                           return x

-- Remove a well-known name from a unique name
remOther n u = do
    a <- gets (Map.lookup u . apps)
    case a of
        Nothing -> throwError $ concat [ "corrupt log: "
                                       , show n
                                       , " released by unknown unique name "
                                       , show u
                                       ]
        Just (x, ns) -> do modifyApps (Map.insert u (x, Set.delete n ns))
                           return x

shape :: Shape -> Renderer ()
shape = tell . (:[])

modifyPending :: (Map Message (Double, Double) -> Map Message (Double, Double))
              -> Renderer ()
modifyPending f = modify $ \bs -> bs { pending = f (pending bs) }

addPending :: Message -> Renderer ()
addPending m = do
    x <- destinationCoordinate m
    y <- gets row
    modifyPending $ Map.insert m (x, y)

findCallCoordinates :: Maybe Message -> Renderer (Maybe (Message, (Double, Double)))
findCallCoordinates = maybe (return Nothing) $ \m -> do
    ret <- gets (Map.lookup m . pending)
    modifyPending $ Map.delete m
    return $ fmap ((,) m) ret

advanceBy :: Double -> Renderer ()
advanceBy d = do
    lastLabelling <- gets mostRecentLabels

    current' <- gets row

    when (current' - lastLabelling > 400) $ do
        xs <- gets (Map.toList . apps)
        let xs' = [ (x, bestNames u os) | (u, (Just x, os)) <- xs ]
        let (height, ss) = headers xs' (current' + 20)
        mapM_ shape ss
        modify $ \bs -> bs { mostRecentLabels = (current' + height + 10)
                           , row = row bs + height + 10
                           }
    current <- gets row
    modify (\bs -> bs { row = row bs + d })
    next <- gets row

    margin <- rightmostApp

    shape $ Rule (margin - 35) (current + 15)

    xs <- gets (catMaybes . Map.fold ((:) . fst) [] . apps)
    forM_ xs $ \x -> shape $ ClientLine x (current + 15) (next + 15)

bestNames :: UniqueName -> Set OtherName -> [String]
bestNames (UniqueName u) os
    | Set.null os = [u]
    | otherwise   = reverse . sortBy (comparing length) . map readable $ Set.toList os
  where readable = reverse . takeWhile (/= '.') . reverse . unOtherName

appCoordinate :: BusName -> Renderer Double
appCoordinate s = getApp s >>= \coord -> case coord of
    Nothing -> throwError $ "corrupt log: contains nonexistant name "
                            ++ unBusName s
    Just x -> return x

rightmostApp :: Renderer Double
rightmostApp = do
    first <- gets nextColumn
    xs <- gets $ catMaybes . map fst . Map.elems . apps
    return $ maximum (first:xs)

senderCoordinate :: Message -> Renderer Double
senderCoordinate m = appCoordinate (sender m)

destinationCoordinate :: Message -> Renderer Double
destinationCoordinate m = appCoordinate (destination m)

memberName :: Message -> Bool -> Renderer ()
memberName message isReturn = do
    current <- gets row
    let Member p i m = member message
        meth = i ++ "." ++ (b m)

    shape $ MemberLabel (it p) (it meth) current
  where it x | isReturn  = "<i>" ++ x ++ "</i>"
             | otherwise = x
        b x  | isReturn  = x
             | otherwise = "<b>" ++ x ++ "</b>"

relativeTimestamp :: Message -> Renderer ()
relativeTimestamp m = do
    base <- gets startTime
    let relative = (timestamp m - base) `div` 1000
    current <- gets row
    shape $ Timestamp (show relative ++ "ms") current

returnArc :: Message -> Double -> Double -> Milliseconds -> Renderer ()
returnArc mr callx cally duration = do
    destinationx <- destinationCoordinate mr
    currentx     <- senderCoordinate mr
    currenty     <- gets row

    shape $ Arc { topx = callx, topy = cally
                , bottomx = currentx, bottomy = currenty
                , arcside = if (destinationx > currentx) then L else R
                , caption = show (duration `div` 1000) ++ "ms"
                }

munge :: Message -> Renderer ()
munge m = case m of
        Signal {}       -> do
            advance
            relativeTimestamp m
            memberName m False
            signal m

        MethodCall {}   -> do
            advance
            relativeTimestamp m
            memberName m False
            methodCall m
            addPending m

        MethodReturn {} -> returnOrError methodReturn
        Error {}        -> returnOrError errorReturn

        Connected { actor = u } -> addUnique u
        Disconnected { actor = u } -> remUnique u >> return ()
        NameChanged { name = n
                    , change = c
                    } -> updateApps_ n c

  where advance = advanceBy 30 -- FIXME: use some function of timestamp
        returnOrError f = do
            call <- findCallCoordinates (inReplyTo m)
            case call of
                Nothing    -> return ()
                Just (m', (x,y)) -> do
                    advance
                    relativeTimestamp m
                    memberName m' True
                    f m
                    let duration = timestamp m - timestamp m'
                    returnArc m x y duration

methodCall, methodReturn, errorReturn :: Message -> Renderer ()
methodCall = methodLike Nothing Above
methodReturn = methodLike Nothing Below
errorReturn = methodLike (Just $ Colour 1 0 0) Below

methodLike :: Maybe Colour -> Arrowhead -> Message -> Renderer ()
methodLike colour a m = do
    sc <- senderCoordinate m
    dc <- destinationCoordinate m
    t <- gets row
    shape $ Arrow colour a sc dc t

signal :: Message -> Renderer ()
signal m = do
    x <- senderCoordinate m
    t <- gets row
    right <- subtract 70 <$> rightmostApp
    let left = 470
    shape $ SignalArrow (left - 20) x (right + 20) t

-- vim: sw=2 sts=2
