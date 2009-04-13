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
import Control.Monad.State
import Control.Monad (forM_)

import Data.List (isPrefixOf, stripPrefix, sortBy)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ord (comparing)

process :: [Message] -> [Shape]
process log =
    let finalState = execState (mapM_ munge log') initialState
    in reverse $ shapes finalState

  where initialState = BustleState Map.empty firstColumn Map.empty 0 0
                                   initTime []
        firstColumn = 470
        relevant (MethodReturn {}) = True
        relevant (Error        {}) = True
        relevant (NameOwnerChanged {}) = True
        relevant m                 = (path . member) m /= "/org/freedesktop/DBus"

        log' = filter relevant log

        initTime = case dropWhile (== 0) (map timestamp log') of
            t:_ -> t
            _   -> 0

type Bustle a = State BustleState a

data BustleState =
    BustleState { apps :: Applications
                , nextColumn :: Double
                , pending :: Map Message (Double, Double)
                , row :: Double
                , mostRecentLabels :: Double
                , startTime :: Milliseconds
                , shapes :: [Shape] -- in reverse order
                }

-- Maps unique connection name to the column representing that name, if
-- allocated, and a set of non-unique names for the connection, if any.
type Applications = Map UniqueName (Maybe Double, Set OtherName)

-- Finds a BusName in a map of applications
lookupApp :: BusName
          -> Applications
          -> Maybe (UniqueName, (Maybe Double, Set OtherName))
lookupApp name as = case name of
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
getApp :: BusName -> Bustle (Maybe Double)
getApp n = do
    app <- gets (lookupApp n . apps)
    case app of
        Nothing -> return Nothing
        Just (u, details) -> Just <$> case details of
            (Just col, _) -> return col
            (Nothing, os) -> assignColumn u os
  where assignColumn :: UniqueName -> Set OtherName -> Bustle Double
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
modifyApps :: (Applications -> Applications) -> Bustle ()
modifyApps f = modify $ \bs -> bs { apps = f (apps bs) }

-- Updates the current set of applications in response to a NameOwnerChanged
-- message.
updateApps :: BusName -- name whose owner has changed.
           -> Maybe BusName -- previous owner, if any.
           -> Maybe BusName -- new owner, if any.
           -> Bustle (Maybe Double, Maybe Double) -- the old and new owners' columns
-- Unique name added, aka. someone connected to the bus
updateApps (U n) Nothing (Just (U m)) | n == m = do addUnique n
                                                    return (Nothing, Nothing)
-- Unique name removed, aka. someone disconnected from the bus
updateApps (U n) (Just (U m)) Nothing | n == m = flip (,) Nothing <$> remUnique n
updateApps (U n) x y = error $ concat [ "corrupt log: unique name ", show n
                                      , " released by ", show x
                                      , "; claimed by " , show y
                                      ]
updateApps (O n) old new = (,) `fmap` maybeM (remOther n) old
                               `ap`   maybeM (addOther n) new

maybeM :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
maybeM = maybe (return Nothing)

-- Adds a new unique name
addUnique :: UniqueName -> Bustle ()
addUnique n = modifyApps $ Map.insert n (Nothing, Set.empty)

-- Removes a unique name, yielding its column if any
remUnique :: UniqueName -> Bustle (Maybe Double)
remUnique n = do
    coord <- gets (fmap fst . Map.lookup n . apps)
    case coord of
      Just x  -> modifyApps (Map.delete n) >> return x
      Nothing -> error $ concat [ "corrupt log: "
                                , show n
                                , " apparently disconnected without connecting"
                                ]

addOther, remOther :: OtherName -> BusName -> Bustle (Maybe Double)
-- Add a new well-known name to a unique name.
addOther n (O o) = error $ concat [ "corrupt log: "
                                  , show n
                                  , " claimed by non-unique name "
                                  , show o
                                  ]
addOther n (U u) = do
    a <- gets (Map.lookup u . apps)
    case a of
        Nothing -> error $ concat [ "corrupt log: "
                                  , show n
                                  , " claimed by unknown unique name "
                                  , show u
                                  ]
        Just (x, ns) -> do modifyApps (Map.insert u (x, Set.insert n ns))
                           return x

-- Remove a well-known name from a unique name
remOther n (O o) = error $ concat [ "corrupt log: "
                                  , show n
                                  , " released by non-unique name "
                                  , show o
                                  ]
remOther n (U u) = do
    a <- gets (Map.lookup u . apps)
    case a of
        Nothing -> error $ concat [ "corrupt log: "
                                  , show n
                                  , " released by unknown unique name "
                                  , show u
                                  ]
        Just (x, ns) -> do modifyApps (Map.insert u (x, Set.delete n ns))
                           return x

shape :: Shape -> Bustle ()
shape s = modify $ \bs -> bs { shapes = s:shapes bs }

modifyPending :: (Map Message (Double, Double) -> Map Message (Double, Double))
              -> Bustle ()
modifyPending f = modify $ \bs -> bs { pending = f (pending bs) }

addPending :: Message -> Bustle ()
addPending m = do
    x <- destinationCoordinate m
    y <- gets row
    modifyPending $ Map.insert m (x, y)

findCallCoordinates :: Maybe Message -> Bustle (Maybe (Message, (Double, Double)))
findCallCoordinates = maybe (return Nothing) $ \m -> do
    ret <- gets (Map.lookup m . pending)
    modifyPending $ Map.delete m
    return $ fmap ((,) m) ret

advanceBy :: Double -> Bustle ()
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

appCoordinate :: BusName -> Bustle Double
-- FIXME: this will break when people try to send methods to non-existant names
appCoordinate s = getApp s >>= \coord -> case coord of
    Nothing -> error "FIXME"
    Just x -> return x

rightmostApp :: Bustle Double
rightmostApp = do
    first <- gets nextColumn
    xs <- gets $ catMaybes . map fst . Map.elems . apps
    return $ maximum (first:xs)

senderCoordinate :: Message -> Bustle Double
senderCoordinate m = appCoordinate (sender m)

destinationCoordinate :: Message -> Bustle Double
destinationCoordinate m = appCoordinate (destination m)

memberName :: Message -> Bustle ()
memberName message = do
    current <- gets row
    let Member p i m = member message
        meth = i ++ ".<b>" ++ m ++ "</b>"

    shape $ MemberLabel p meth current

relativeTimestamp :: Message -> Bustle ()
relativeTimestamp m = do
    base <- gets startTime
    let relative = (timestamp m - base) `div` 1000
    current <- gets row
    shape $ Timestamp (show relative ++ "ms") current

returnArc :: Message -> Double -> Double -> Milliseconds -> Bustle ()
returnArc mr callx cally duration = do
    destinationx <- destinationCoordinate mr
    currentx     <- senderCoordinate mr
    currenty     <- gets row

    shape $ Arc { topx = callx, topy = cally
                , bottomx = currentx, bottomy = currenty
                , arcside = if (destinationx > currentx) then L else R
                , caption = show (duration `div` 1000) ++ "ms"
                }

munge :: Message -> Bustle ()
munge m = case m of
        Signal {}       -> do
            advance
            relativeTimestamp m
            memberName m
            signal m

        MethodCall {}   -> do
            advance
            relativeTimestamp m
            memberName m
            methodCall m
            addPending m

        MethodReturn {} -> returnOrError methodReturn
        Error {}        -> returnOrError errorReturn
        NameOwnerChanged { changedName = c
                         , oldOwner = o
                         , newOwner = n
                         } -> updateApps c o n >> return ()
  where advance = advanceBy 30 -- FIXME: use some function of timestamp
        returnOrError f = do
            call <- findCallCoordinates (inReplyTo m)
            case call of
                Nothing    -> return ()
                Just (m', (x,y)) -> do
                    advance
                    relativeTimestamp m
                    f m
                    let duration = timestamp m - timestamp m'
                    returnArc m x y duration


methodCall, methodReturn, errorReturn :: Message -> Bustle ()
methodCall = methodLike Nothing Above
methodReturn = methodLike Nothing Below
errorReturn = methodLike (Just $ Colour 1 0 0) Below

methodLike :: Maybe Colour -> Arrowhead -> Message -> Bustle ()
methodLike colour a m = do
    sc <- senderCoordinate m
    dc <- destinationCoordinate m
    t <- gets row
    shape $ Arrow colour a sc dc t

signal :: Message -> Bustle ()
signal m = do
    x <- senderCoordinate m
    t <- gets row
    right <- subtract 70 <$> rightmostApp
    let left = 470
    shape $ SignalArrow (left - 20) x (right + 20) t

-- vim: sw=2 sts=2
