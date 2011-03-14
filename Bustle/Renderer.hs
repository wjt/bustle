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

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Arrow ((&&&))
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad (forM_)

import Data.List (isPrefixOf, stripPrefix, sort, sortBy)
import Data.Maybe (fromJust, maybe, fromMaybe, catMaybes)
import Data.Ord (comparing)

data Bus = SessionBus
         | SystemBus
    deriving (Show, Eq, Ord)

describeBus :: Bus -> String
describeBus SessionBus = "session"
describeBus SystemBus = "system"

process :: [Message] -> [Message] -> ((Double, [Shape]), [String])
process sessionBusLog systemBusLog =
    (topLeftJustifyDiagram diagram, ws)
  where
        (diagram, ws) = runRenderer (mapM_ (uncurry munge) log')
                                          (initialState initTime)

        -- FIXME: really? Maybe we should allow people to be interested in,
        --        say, binding to signals?
        senderIsBus m = sender m == O (OtherName "org.freedesktop.DBus")
        destIsBus m = destination m == O (OtherName "org.freedesktop.DBus")

        -- When the monitor is forcibly disconnected from the bus, the
        -- Disconnected message has no sender, so the logger spits out <none>.
        isDisconnected m = sender m == O (OtherName "<none>")

        relevant m@(Signal {}) = not . any ($ m) $ [ senderIsBus
                                                   , isDisconnected
                                                   ]
        relevant m@(MethodCall {}) = not . any ($ m) $ [ senderIsBus
                                                       , destIsBus
                                                       , isDisconnected
                                                       ]
        relevant _ = True

        log' = filter (relevant . snd) $ combine sessionBusLog systemBusLog

        initTime = case dropWhile (== 0) (map (timestamp . snd) log') of
            t:_ -> t
            _   -> 0

-- Combines a series of messages on the session bus and system bus into a
-- single ordered list, annotated by timestamp. Assumes both the source lists
-- are sorted.
combine :: [Message] -- ^ session bus messages
        -> [Message] -- ^ system bus messages
        -> [(Bus, Message)]
combine [] [] = []
combine xs [] = zip (repeat SessionBus) xs
combine [] ys = zip (repeat SystemBus) ys
combine xs@(x:xs') ys@(y:ys') =
    if timestamp x < timestamp y
        then (SessionBus, x):combine xs' ys
        else (SystemBus, y):combine xs ys'

newtype Renderer a = Renderer (WriterT [Shape]
                                (StateT RendererState Identity)
                                a)
  deriving (Functor, Monad, MonadState RendererState, MonadWriter [Shape])

instance Applicative Renderer where
    pure = return
    (<*>) = ap

runRenderer :: Renderer () -> RendererState -> ([Shape], [String])
runRenderer (Renderer act) st = runIdentity $ do
    (result, st') <- runStateT (execWriterT act) st
    return (result, reverse (warnings st'))

data BusState = BusState { apps :: Applications
                         , firstColumn :: Double
                         , nextColumn :: Double
                         , pending :: Pending
                         }

data RendererState =
    RendererState { sessionBusState :: BusState
                  , systemBusState :: BusState
                  , row :: Double
                  , mostRecentLabels :: Double
                  , startTime :: Milliseconds
                  , warnings :: [String]
                  }

initialBusState :: Double ->  BusState
initialBusState first =
    BusState { apps = Map.empty
             , firstColumn = first
             , nextColumn = first
             , pending = Map.empty
             }

initialSessionBusState, initialSystemBusState :: BusState
initialSessionBusState =
    initialBusState $ timestampAndMemberWidth + firstColumnOffset
initialSystemBusState =
    initialBusState $ negate firstColumnOffset

initialState :: Milliseconds -> RendererState
initialState t = RendererState
    { sessionBusState = initialSessionBusState
    , systemBusState = initialSystemBusState
    , row = 0
    , mostRecentLabels = 0
    , startTime = t
    , warnings = []
    }

-- Maps unique connection name to the column representing that name, if
-- allocated, and a set of non-unique names for the connection, if any.
type Applications = Map UniqueName (Maybe Double, Set OtherName)

-- Map from a method call message to the coordinates at which the arc to its
-- return should start.
type Pending = Map Message (Double, Double)

getBusState :: Bus -> Renderer BusState
getBusState = getsBusState id

getsBusState :: (BusState -> a) -> Bus -> Renderer a
getsBusState f SessionBus = gets (f . sessionBusState)
getsBusState f SystemBus = gets (f . systemBusState)

modifyBusState :: Bus -> (BusState -> BusState) -> Renderer ()
modifyBusState bus f = case bus of
    SessionBus -> modify $ \rs -> rs { sessionBusState = f (sessionBusState rs)
                                     }
    SystemBus -> modify $ \rs -> rs { systemBusState = f (systemBusState rs)
                                    }

getApps :: Bus -> Renderer Applications
getApps bus = apps <$> getBusState bus

getsApps :: (Applications -> a) -> Bus -> Renderer a
getsApps f = getsBusState (f . apps)

lookupUniqueName :: Bus
                 -> UniqueName
                 -> Renderer (Maybe Double, Set OtherName)
lookupUniqueName bus u = do
    thing <- getsApps (Map.lookup u) bus
    case thing of
        Just nameInfo -> return nameInfo
        Nothing       -> do
            warn $ concat [ "'"
                          , unUniqueName u
                          , "' appeared unheralded on the "
                          , describeBus bus
                          , " bus."
                          ]
            addUnique bus u
            return (Nothing, Set.empty)

lookupOtherName :: Bus
                -> OtherName
                -> Renderer (UniqueName, (Maybe Double, Set OtherName))
lookupOtherName bus o = do
    as <- getApps bus
    case filter (Set.member o . snd . snd) (Map.assocs as) of
        [details] -> return details

        -- No matches indicates a corrupt log, which we try to recover from …
        []        -> do
            warn $ concat [ "'"
                          , unOtherName o
                          , "' appeared unheralded on the "
                          , describeBus bus
                          , " bus; making something up..."
                          ]
            let namesInUse = Map.keys as
                candidates = map (UniqueName . (":0." ++) . show)
                                 ([1..] :: [Integer])
                u = head $ filter (not . (`elem` namesInUse)) candidates
            addUnique bus u
            maybeCoord <- addOther bus o u
            return (u, (maybeCoord, Set.singleton o))

        -- … but more than one match means we're screwed.
        several   -> error $ concat [ "internal error: "
                                    , show o
                                    , " in several apps: "
                                    , show several
                                    ]

-- Finds a BusName in a map of applications
lookupApp :: Bus
          -> BusName
          -> Renderer (UniqueName, (Maybe Double, Set OtherName))
lookupApp bus name = case name of
    U u -> do
        details <- lookupUniqueName bus u
        return (u, details)
    O o -> lookupOtherName bus o

-- Finds a BusName in the current state, yielding its column if it exists.  If
-- it exists, but previously lacked a column, a column is allocated.
appCoordinate :: Bus -> BusName -> Renderer Double
appCoordinate bus n = do
    (u, details) <- lookupApp bus n
    case details of
        (Just col, _) -> return col
        (Nothing, os) -> assignColumn u os
  where assignColumn :: UniqueName -> Set OtherName -> Renderer Double
        assignColumn u os = do
            x <- nextColumn <$> getBusState bus

            -- FIXME: ick
            let f = case bus of
                    SessionBus -> (+ columnWidth)
                    SystemBus -> subtract columnWidth
            modifyBusState bus $ \bs -> bs { nextColumn = f x }
            modifyApps bus $ Map.insert u (Just x, os)

            -- FIXME: Does this really live here?
            currentRow <- gets row
            let ns = bestNames u os
                h  = headerHeight ns
            shape $ Header ns x (currentRow - (10 + h))
            shape $ ClientLine x (currentRow - 5) (currentRow + 15)

            return x

-- Modify the application table directly.
modifyApps :: Bus -> (Applications -> Applications) -> Renderer ()
modifyApps bus f = modifyBusState bus $ \bs -> bs { apps = f (apps bs) }

-- Updates the current set of applications in response to a well-known name's
-- owner changing.
updateApps :: Bus -- ^ bus on which a name's owner has changed
           -> OtherName -- name whose owner has changed.
           -> Change -- details of the change
           -> Renderer (Maybe Double, Maybe Double) -- the old and new owners' columns
updateApps bus n c = case c of
    Claimed new -> (,) Nothing `fmap` addOther bus n new
    Stolen old new -> (,) `fmap` remOther bus n old `ap` addOther bus n new
    Released old -> flip (,) Nothing `fmap` remOther bus n old

-- updateApps but ignore the reply.
updateApps_ :: Bus -- ^ bus on which a name's owner has changed
            -> OtherName -- name whose owner has changed.
            -> Change -- details of the change
            -> Renderer ()
updateApps_ bus n c = updateApps bus n c >> return ()

-- Adds a new unique name
addUnique :: Bus -> UniqueName -> Renderer ()
addUnique bus n = modifyApps bus $ Map.insert n (Nothing, Set.empty)
  -- FIXME: this could trample on names that erroneously already exist...

-- Removes a unique name, yielding its column (if any)
remUnique :: Bus -> UniqueName -> Renderer (Maybe Double)
remUnique bus n = do
    coord <- fst <$> lookupUniqueName bus n
    modifyApps bus (Map.delete n)
    return coord

addOther, remOther :: Bus -> OtherName -> UniqueName -> Renderer (Maybe Double)
-- Add a new well-known name to a unique name.
addOther bus n u = do
    (x, ns) <- lookupUniqueName bus u
    modifyApps bus (Map.insert u (x, Set.insert n ns))
    return x

-- Remove a well-known name from a unique name
remOther bus n u = do
    (x, ns) <- lookupUniqueName bus u
    modifyApps bus (Map.insert u (x, Set.delete n ns))
    return x

shape :: Shape -> Renderer ()
shape = tell . (:[])

warn :: String -> Renderer ()
warn warning = modify $ \rs -> rs { warnings = warning:warnings rs }

modifyPending :: Bus
              -> (Pending -> Pending)
              -> Renderer ()
modifyPending bus f = modifyBusState bus $ \bs ->
    bs { pending = f (pending bs) }

addPending :: Bus -> Message -> Renderer ()
addPending bus m = do
    x <- destinationCoordinate bus m
    y <- gets row
    modifyPending bus $ Map.insert m (x, y)

findCallCoordinates :: Bus
                    -> Maybe Message
                    -> Renderer (Maybe (Message, (Double, Double)))
findCallCoordinates bus = maybe (return Nothing) $ \m -> do
    ret <- getsBusState (Map.lookup m . pending) bus
    modifyPending bus $ Map.delete m
    return $ fmap ((,) m) ret

advanceBy :: Double -> Renderer ()
advanceBy d = do
    lastLabelling <- gets mostRecentLabels

    current' <- gets row

    when (current' - lastLabelling > 400) $ do
        xs <- (++) <$> getsApps Map.toList SessionBus
                   <*> getsApps Map.toList SystemBus
        let xs' = [ (x, bestNames u os) | (u, (Just x, os)) <- xs ]
        let (height, ss) = headers xs' (current' + 20)
        mapM_ shape ss
        modify $ \bs -> bs { mostRecentLabels = (current' + height + 10)
                           , row = row bs + height + 10
                           }
    current <- gets row
    modify (\bs -> bs { row = row bs + d })
    next <- gets row

    -- The adjustments here leave space for a new app's headers to be drawn
    -- without overlapping the rule.
    leftMargin <- maybe 0 (+ 35) <$> edgemostApp SystemBus
    rightMargin <- maybe timestampAndMemberWidth (subtract 35)
                   <$> edgemostApp SessionBus
    shape $ Rule leftMargin rightMargin (current + 15)

    let appColumns :: Applications -> [Double]
        appColumns = catMaybes . Map.fold ((:) . fst) []
    xs <- (++) <$> getsApps appColumns SessionBus
               <*> getsApps appColumns SystemBus
    forM_ xs $ \x -> shape $ ClientLine x (current + 15) (next + 15)

bestNames :: UniqueName -> Set OtherName -> [String]
bestNames (UniqueName u) os
    | Set.null os = [u]
    | otherwise   = reverse . sortBy (comparing length) . map readable $ Set.toList os
  where readable = reverse . takeWhile (/= '.') . reverse . unOtherName

edgemostApp :: Bus -> Renderer (Maybe Double)
edgemostApp bus = do
    (first, next) <- getsBusState (firstColumn &&& nextColumn) bus
    xs <- getsApps (catMaybes . map fst . Map.elems) bus

    -- FIXME: per-bus sign
    let edgiest = case bus of
            SessionBus -> maximum
            SystemBus -> minimum

    if first == next
        then return Nothing
        -- FIXME: Including 'next' here seems to fix the rendering of signals
        -- which are the first appearence on the chart of a name on the system
        -- bus.  This is a hack but I CBA to figure out why.
        else return $ Just $ edgiest (next:xs)

senderCoordinate :: Bus -> Message -> Renderer Double
senderCoordinate bus m = appCoordinate bus (sender m)

destinationCoordinate :: Bus -> Message -> Renderer Double
destinationCoordinate bus m = appCoordinate bus (destination m)

memberName :: Message -> Bool -> Renderer ()
memberName message isReturn = do
    current <- gets row
    let Member p i m = member message
        meth = i ++ "." ++ (b m)

    shape $ memberLabel (it p) (it meth) current
  where it x | isReturn  = "<i>" ++ x ++ "</i>"
             | otherwise = x
        b x  | isReturn  = x
             | otherwise = "<b>" ++ x ++ "</b>"

relativeTimestamp :: Message -> Renderer ()
relativeTimestamp m = do
    base <- gets startTime
    let relative = (timestamp m - base) `div` 1000
    current <- gets row
    shape $ timestampLabel (show relative ++ "ms") current

returnArc :: Bus -> Message -> Double -> Double -> Milliseconds -> Renderer ()
returnArc bus mr callx cally duration = do
    destinationx <- destinationCoordinate bus mr
    currentx     <- senderCoordinate bus mr
    currenty     <- gets row

    shape $ Arc { topx = callx, topy = cally
                , bottomx = currentx, bottomy = currenty
                , arcside = if (destinationx > currentx) then L else R
                , caption = show (duration `div` 1000) ++ "ms"
                }

munge :: Bus -> Message -> Renderer ()
munge bus m = case m of
        Signal {}       -> do
            advance
            relativeTimestamp m
            memberName m False
            signal bus m

        MethodCall {}   -> do
            advance
            relativeTimestamp m
            memberName m False
            methodCall bus m
            addPending bus m

        MethodReturn {} -> returnOrError $ methodReturn bus
        Error {}        -> returnOrError $ errorReturn bus

        Connected { actor = u } -> addUnique bus u
        Disconnected { actor = u } -> remUnique bus  u >> return ()
        NameChanged { changedName = n
                    , change = c
                    } -> updateApps_ bus n c

  where advance = advanceBy 30 -- FIXME: use some function of timestamp
        returnOrError f = do
            call <- findCallCoordinates bus (inReplyTo m)
            case call of
                Nothing    -> return ()
                Just (m', (x,y)) -> do
                    advance
                    relativeTimestamp m
                    memberName m' True
                    f m
                    let duration = timestamp m - timestamp m'
                    returnArc bus m x y duration

methodCall, methodReturn, errorReturn :: Bus -> Message -> Renderer ()
methodCall = methodLike Nothing Above
methodReturn = methodLike Nothing Below
errorReturn = methodLike (Just $ Colour 1 0 0) Below

methodLike :: Maybe Colour -> Arrowhead -> Bus -> Message -> Renderer ()
methodLike colour a bus m = do
    sc <- senderCoordinate bus m
    dc <- destinationCoordinate bus m
    t <- gets row
    shape $ Arrow colour a sc dc t

signal :: Bus -> Message -> Renderer ()
signal bus m = do
    x <- senderCoordinate bus m
    t <- gets row

    -- FIXME: per-bus sign.
    let f = case bus of
            SessionBus -> subtract
            SystemBus  -> (+)
    -- fromJust is safe here because we must have an app to have a signal. It
    -- doesn't make me very happy though.
    outside <- f columnWidth . fromJust <$> edgemostApp bus
    inside <- getsBusState firstColumn bus
    let [x1, x2] = sort [outside, inside]

    shape $ SignalArrow (x1 - 20) x (x2 + 20) t

-- vim: sw=2 sts=2
