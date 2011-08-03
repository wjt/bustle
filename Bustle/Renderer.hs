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
    (
      process
    , RendererResult(..)
    )
where

import Prelude hiding (log)

import Bustle.Types
import Bustle.Diagram
import Bustle.Regions

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

data RendererResult =
    RendererResult { rrCentreOffset :: Double
                   , rrShapes :: [Shape]
                   , rrRegions :: Regions DetailedMessage
                   , rrWarnings :: [String]
                   }

process :: Log
        -> Log
        -> RendererResult
process sessionBusLog systemBusLog =
    RendererResult x diagram' regions' (reverse $ warnings rs)
  where
        ((diagram, messageRegions), rs) = runRenderer (mapM_ (uncurry munge) log')
                                          (initialState initTime)
        (_translation@(x, y), diagram') = topLeftJustifyDiagram diagram
        regions' = translateRegions y messageRegions

        log' = combine sessionBusLog systemBusLog

        timestamps = map (dmTimestamp . snd) log'
        initTime = case dropWhile (== 0) timestamps of
            t:_ -> t
            _   -> 0

-- Combines a series of messages on the session bus and system bus into a
-- single ordered list, annotated by timestamp. Assumes both the source lists
-- are sorted.
combine :: Log -- ^ session bus messages
        -> Log -- ^ system bus messages
        -> [(Bus, DetailedMessage)]
combine [] [] = []
combine xs [] = zip (repeat SessionBus) xs
combine [] ys = zip (repeat SystemBus) ys
combine xs@(x:xs') ys@(y:ys') =
    if dmTimestamp x < dmTimestamp y
        then (SessionBus, x):combine xs' ys
        else (SystemBus, y):combine xs ys'

newtype Renderer a =
    Renderer (WriterT ([Shape], Regions DetailedMessage)
                 (StateT (RendererState) Identity)
                    a)
  deriving ( Functor
           , Monad
           , MonadState (RendererState)
           , MonadWriter ([Shape], Regions DetailedMessage)
           )

instance Applicative Renderer where
    pure = return
    (<*>) = ap

runRenderer :: Renderer ()
            -> RendererState
            -> ( ([Shape], Regions DetailedMessage)
               , RendererState
               )
runRenderer (Renderer act) st = runIdentity $ runStateT (execWriterT act) st

data BusState =
    BusState { apps :: Applications
             , firstColumn :: Double
             , nextColumn :: Double
             , pending :: Pending
             }

data RendererState =
    RendererState { sessionBusState :: BusState
                  , systemBusState :: BusState
                  , row :: Double
                  , mostRecentLabels :: Double
                  , startTime :: Microseconds
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

initialState :: Microseconds -> RendererState
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
data ApplicationInfo =
    ApplicationInfo { aiColumn :: Maybe Double
                    , aiCurrentNames :: Set OtherName
                    }
  deriving
    Show
type Applications = Map UniqueName ApplicationInfo

-- Map from a method call message to the coordinates at which the arc to its
-- return should start.
type Pending = Map DetailedMessage (Double, Double)

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
                 -> Renderer ApplicationInfo
lookupUniqueName bus u = do
    thing <- getsApps (Map.lookup u) bus
    case thing of
        Just nameInfo -> return nameInfo
        -- This happens with pcap logs where we don't (currently) have
        -- explicit change notification for unique names in the stream of
        -- DetailedMessages.
        Nothing       -> addUnique bus u

lookupOtherName :: Bus
                -> OtherName
                -> Renderer (UniqueName, ApplicationInfo)
lookupOtherName bus o = do
    as <- getApps bus
    case filter (Set.member o . aiCurrentNames . snd) (Map.assocs as) of
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
                candidates = map (UniqueName . (":fake." ++) . show)
                                 ([1..] :: [Integer])
                u = head $ filter (not . (`elem` namesInUse)) candidates
            addUnique bus u
            addOther bus o u
            ai <- lookupUniqueName bus u
            return (u, ai)

        -- … but more than one match means we're screwed.
        several   -> error $ concat [ "internal error: "
                                    , show o
                                    , " in several apps: "
                                    , show several
                                    ]

-- Finds a BusName in a map of applications
lookupApp :: Bus
          -> BusName
          -> Renderer (UniqueName, ApplicationInfo)
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
    case aiColumn details of
        Just col -> return col
        Nothing  -> assignColumn u (aiCurrentNames details)
  where assignColumn :: UniqueName -> Set OtherName -> Renderer Double
        assignColumn u os = do
            x <- nextColumn <$> getBusState bus

            -- FIXME: ick
            let f = case bus of
                    SessionBus -> (+ columnWidth)
                    SystemBus -> subtract columnWidth
            modifyBusState bus $ \bs -> bs { nextColumn = f x }
            modifyApps bus $ Map.adjust (\ai -> ai { aiColumn = Just x }) u

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
           -> Renderer ()
updateApps bus n c = case c of
    Claimed new -> addOther bus n new
    Stolen old new -> remOther bus n old >> addOther bus n new
    Released old -> remOther bus n old

-- Adds a new unique name
addUnique :: Bus -> UniqueName -> Renderer ApplicationInfo
addUnique bus n = do
    let ai = ApplicationInfo Nothing Set.empty
    -- FIXME: this could trample on names that erroneously already exist...
    modifyApps bus $ Map.insert n ai
    return ai

-- Removes a unique name
remUnique :: Bus -> UniqueName -> Renderer ()
remUnique bus n = do
    modifyApps bus (Map.delete n)

addOther, remOther :: Bus -> OtherName -> UniqueName -> Renderer ()
-- Add a new well-known name to a unique name.
addOther bus n u = do
    ai <- lookupUniqueName bus u
    let ai' = ai { aiCurrentNames = Set.insert n (aiCurrentNames ai) }
    modifyApps bus $ Map.insert u ai'

-- Remove a well-known name from a unique name
remOther bus n u = do
    ai <- lookupUniqueName bus u
    let ai' = ai { aiCurrentNames = Set.delete n (aiCurrentNames ai) }
    modifyApps bus $ Map.insert u ai'

shape :: Shape -> Renderer ()
shape s = tell ([s], [])

region :: Stripe -> DetailedMessage -> Renderer ()
region r m = tell ([], [(r, m)])

warn :: String -> Renderer ()
warn warning = modify $ \rs -> rs { warnings = warning:warnings rs }

modifyPending :: Bus
              -> (Pending -> Pending)
              -> Renderer ()
modifyPending bus f = modifyBusState bus $ \bs ->
    bs { pending = f (pending bs) }

addPending :: Bus
           -> DetailedMessage
           -> Renderer ()
addPending bus m = do
    x <- destinationCoordinate bus m
    y <- gets row
    modifyPending bus $ Map.insert m (x, y)

findCallCoordinates :: Bus
                    -> Maybe DetailedMessage
                    -> Renderer (Maybe (DetailedMessage, (Double, Double)))
findCallCoordinates bus = maybe (return Nothing) $ \m -> do
    ret <- getsBusState (Map.lookup m . pending) bus
    modifyPending bus $ Map.delete m
    return $ fmap ((,) m) ret

-- The adjustments here leave space for a new app's headers to be drawn
-- without overlapping the rule.
getLeftMargin, getRightMargin :: Renderer Double
getLeftMargin =
    maybe 0 (+ 35) <$> edgemostApp SystemBus
getRightMargin =
    maybe timestampAndMemberWidth (subtract 35) <$> edgemostApp SessionBus

advanceBy :: Double -> Renderer ()
advanceBy d = do
    lastLabelling <- gets mostRecentLabels

    current' <- gets row

    when (current' - lastLabelling > 400) $ do
        xs <- (++) <$> getsApps Map.toList SessionBus
                   <*> getsApps Map.toList SystemBus
        let xs' = [ (x, bestNames u os) | (u, ApplicationInfo (Just x) os) <- xs ]
        let (height, ss) = headers xs' (current' + 20)
        mapM_ shape ss
        modify $ \bs -> bs { mostRecentLabels = (current' + height + 10)
                           , row = row bs + height + 10
                           }
    current <- gets row
    modify (\bs -> bs { row = row bs + d })
    next <- gets row

    leftMargin <- getLeftMargin
    rightMargin <- getRightMargin
    shape $ Rule leftMargin rightMargin (current + 15)

    let appColumns :: Applications -> [Double]
        appColumns = catMaybes . Map.fold ((:) . aiColumn) []
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
    xs <- getsApps (catMaybes . map aiColumn . Map.elems) bus

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

senderCoordinate :: Bus
                 -> DetailedMessage
                 -> Renderer Double
senderCoordinate bus m = appCoordinate bus . sender $ dmMessage m

destinationCoordinate :: Bus
                      -> DetailedMessage
                      -> Renderer Double
destinationCoordinate bus m = appCoordinate bus . destination $ dmMessage m

memberName :: DetailedMessage
           -> Bool
           -> Renderer ()
memberName message isReturn = do
    current <- gets row
    let Member p i m = member $ dmMessage message
    shape $ memberLabel p i m isReturn current

relativeTimestamp :: DetailedMessage -> Renderer ()
relativeTimestamp dm = do
    base <- gets startTime
    let relative = µsToMs (dmTimestamp dm - base)
    current <- gets row
    shape $ timestampLabel (show relative ++ "ms") current

returnArc :: Bus
          -> DetailedMessage
          -> Double
          -> Double
          -> Microseconds
          -> Renderer ()
returnArc bus mr callx cally duration = do
    destinationx <- destinationCoordinate bus mr
    currentx     <- senderCoordinate bus mr
    currenty     <- gets row

    shape $ Arc { topx = callx, topy = cally
                , bottomx = currentx, bottomy = currenty
                , arcside = if (destinationx > currentx) then L else R
                , caption = show (µsToMs duration) ++ "ms"
                }

addMessageRegion :: DetailedMessage
                 -> Renderer ()
addMessageRegion m = do
    newRow <- gets row

    -- FIXME: wtf. "row" points to the ... middle ... of the current row.
    region (Stripe (newRow - eventHeight / 2) (newRow + eventHeight / 2)) m

munge :: Bus
      -> DetailedMessage
      -> Renderer ()
munge bus dm@(DetailedMessage _ m _) =
    case m of
        Signal {}       -> do
            advance
            relativeTimestamp dm
            memberName dm False
            signal bus dm
            addMessageRegion dm

        MethodCall {}   -> do
            advance
            relativeTimestamp dm
            memberName dm False
            methodCall bus dm
            addPending bus dm
            addMessageRegion dm

        MethodReturn {} -> returnOrError $ methodReturn bus
        Error {}        -> returnOrError $ errorReturn bus

        Connected { actor = u } -> addUnique bus u >> return ()
        Disconnected { actor = u } -> remUnique bus u
        NameChanged { changedName = n
                    , change = c
                    } -> updateApps bus n c

  where advance = advanceBy eventHeight -- FIXME: use some function of timestamp?
        returnOrError f = do
            call <- findCallCoordinates bus (inReplyTo m)
            case call of
                Nothing    -> return ()
                Just (dm', (x,y)) -> do
                    advance
                    relativeTimestamp dm
                    memberName dm' True
                    f dm
                    let duration = dmTimestamp dm - dmTimestamp dm'
                    returnArc bus dm x y duration
                    addMessageRegion dm

methodCall, methodReturn, errorReturn :: Bus
                                      -> DetailedMessage
                                      -> Renderer ()
methodCall = methodLike Nothing Above
methodReturn = methodLike Nothing Below
errorReturn = methodLike (Just $ Colour 1 0 0) Below

methodLike :: Maybe Colour
           -> Arrowhead
           -> Bus
           -> DetailedMessage
           -> Renderer ()
methodLike colour a bus dm = do
    sc <- senderCoordinate bus dm
    dc <- destinationCoordinate bus dm
    t <- gets row
    shape $ Arrow colour a sc dc t

signal :: Bus -> DetailedMessage -> Renderer ()
signal bus dm = do
    x <- senderCoordinate bus dm
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
