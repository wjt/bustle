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

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)

process :: [Message] -> [Shape]
process log =
    let finalState = execState (mapM_ munge log') initialState
    in reverse $ shapes finalState

  where initialState = BustleState Map.empty firstColumn Map.empty Map.empty 0 0
                                   initTime []
        firstColumn = 470
        relevant (MethodReturn {}) = True
        relevant (Error        {}) = True
        relevant (NameOwnerChanged {}) = True
        relevant m                 = (path . member) m /= "/org/freedesktop/DBus"

        log' = filter relevant log

        initTime = case log' of
            m:_ -> timestamp m
            _   -> 0

type Bustle a = State BustleState a

data BustleState =
    BustleState { apps :: Applications
                , nextColumn :: Double
                , coordinates :: Map BusName Double
                , pending :: Map Message (Double, Double)
                , row :: Double
                , mostRecentLabels :: Double
                , startTime :: Milliseconds
                , shapes :: [Shape] -- in reverse order
                }

modifyCoordinates :: (Map BusName Double -> Map BusName Double) -> Bustle ()
modifyCoordinates f = modify (\bs -> bs { coordinates = f (coordinates bs)})

-- Maps unique connection name to the column representing that name, and a set
-- of non-unique names for the connection, if any.
-- XXX this can't represent messages to unowned names. I guess we can just draw
-- those arrows as going to a question mark? That's something to consider
-- whenever a BusName is looked up in this map. Indeed this problem existed
-- before.
-- XXX lazily allocate coordinate
type Applications = Map UniqueName (Double, Set OtherName)

-- Finds a BusName in a map of applications, yielding its column (if any)
lookupApp :: BusName -> Applications -> Maybe Double
lookupApp name as = case name of
    U u -> fst <$> Map.lookup u as
    O o -> case filter (Set.member o . snd) (Map.elems as) of
              []         -> Nothing
              [(col, _)] -> Just col
              several    -> error $ concat [ "internal error: "
                                           , show o
                                           , " in several apps: "
                                           , show several
                                           ]

-- Finds a BusName in the current state, yielding its column (if any)
getApp :: BusName -> Bustle (Maybe Double)
getApp n = gets (lookupApp n . apps)

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
updateApps (U n) Nothing (Just (U m)) | n == m = (,) Nothing . Just <$> addUnique n
-- Unique name removed, aka. someone disconnected from the bus
updateApps (U n) (Just (U m)) Nothing | n == m = flip (,) Nothing . Just <$> remUnique n
updateApps (U n) x y = error $ concat [ "corrupt log: unique name ", show n
                                      , " released by ", show x
                                      , "; claimed by " , show y
                                      ]
updateApps (O n) old new = (,) `fmap` notfmap (remOther n) old
                               `ap`   notfmap (addOther n) new

-- This is secretly traverse but State is not Applicative. Angerous rage.
notfmap :: (a -> Bustle b) -> Maybe a -> Bustle (Maybe b)
notfmap f = maybe (return Nothing) (liftM Just . f)

addUnique, remUnique :: UniqueName -> Bustle Double
-- Adds a new unique name, yielding its column
addUnique n = do
    x <- gets nextColumn
    modify $ \bs -> bs { nextColumn = x + 70 }
    modifyApps $ Map.insert n (x, Set.empty)
    return x

-- Removes a unique name, yielding its column
remUnique n = do
    coord <- gets (fmap fst . Map.lookup n . apps)
    case coord of
      Just x  -> modifyApps (Map.delete n) >> return x
      Nothing -> error $ concat [ "corrupt log: "
                                , show n
                                , " apparently disconnected without connecting"
                                ]

addOther, remOther :: OtherName -> BusName -> Bustle Double
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
        xs <- gets (Map.toList . coordinates)
        forM_ xs $ \(name, x) ->
          shape $ Header (abbreviateBusName name) x (current' + d)
        modify $ \bs -> bs { mostRecentLabels = (current' + d)
                           , row = row bs + d
                           }
    current <- gets row
    modify (\bs -> bs { row = row bs + d })
    next <- gets row

    margin <- rightmostApp

    shape $ Rule (margin + 35) (current + 15)

    xs <- gets (Map.fold (:) [] . coordinates)
    forM_ xs $ \x -> shape $ ClientLine x (current + 15) (next + 15)

abbreviateBusName :: BusName -> String
abbreviateBusName (U (UniqueName n)) = n
abbreviateBusName (O (OtherName  n)) = reverse . takeWhile (/= '.') . reverse $ n

addApplication :: BusName -> Double -> Bustle Double
addApplication s c = do
    currentRow <- gets row

    shape $ Header (abbreviateBusName s) c (currentRow - 20)
    shape $ ClientLine c (currentRow - 5) (currentRow + 15)

    modifyCoordinates (Map.insert s c)
    return c

firstAppX :: Double
firstAppX = 400

appCoordinate :: BusName -> Bustle Double
appCoordinate s = do
    cs <- gets coordinates
    case Map.lookup s cs of
        Just c  -> return c
        Nothing -> do c <- rightmostApp
                      addApplication s (c + 70)

rightmostApp :: Bustle Double
rightmostApp = Map.fold max firstAppX `fmap` gets coordinates

senderCoordinate :: Message -> Bustle Double
senderCoordinate m = appCoordinate (sender m)

destinationCoordinate :: Message -> Bustle Double
destinationCoordinate m = appCoordinate (destination m)

abbreviate :: Interface -> Interface
abbreviate i = fromMaybe i $ stripPrefix "org.freedesktop." i

prettyPath :: ObjectPath -> ObjectPath
prettyPath p = fromMaybe p $ stripPrefix "/org/freedesktop/Telepathy/Connection/" p

memberName :: Message -> Bustle ()
memberName message = do
    current <- gets row
    let Member p i m = member message
        p' = prettyPath p
        meth = abbreviate $ i ++ "." ++ m

    shape $ MemberLabel p' meth current

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
    cs <- gets coordinates
    let (left, right) = (Map.fold min 10000 cs, Map.fold max 0 cs)
    shape $ SignalArrow (left - 20) x (right + 20) t

-- vim: sw=2 sts=2
