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

import Control.Monad.State
import Control.Monad (forM_)

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)

--process :: [Message] -> (Double, Double, Render ())
process log =
  let (w, h, ss) = process' log
      act = mapM_ draw ss
  in (w, h, act)

process' :: [Message] -> (Double, Double, [Shape])
process' log =
    let finalState = execState (mapM_ munge log') initialState
        width = Map.fold max firstAppX (coordinates finalState) + 70
        height = row finalState + 30
    in (width, height, reverse (shapes finalState))

  where initialState = BustleState Map.empty Map.empty 0 0 initTime []
        relevant (MethodReturn {}) = True
        relevant (Error        {}) = True
        relevant m                 = path m /= "/org/freedesktop/DBus"

        log' = filter relevant log

        initTime = case log' of
            m:_ -> timestamp m
            _   -> 0

data BustleState =
    BustleState { coordinates :: Map BusName Double
                , pending :: Map (BusName, Serial) (Message, (Double, Double))
                , row :: Double
                , mostRecentLabels :: Double
                , startTime :: Milliseconds
                , shapes :: [Shape] -- in reverse order
                }

type Bustle a = State BustleState a

modifyCoordinates :: (Map BusName Double -> Map BusName Double) -> Bustle ()
modifyCoordinates f = modify (\bs -> bs { coordinates = f (coordinates bs)})

shape :: Shape -> Bustle ()
shape s = modify $ \bs -> bs { shapes = s:shapes bs }

addPending :: Message -> Bustle ()
addPending m = do
    x <- destinationCoordinate m
    y <- gets row
    let update = Map.insert (sender m, serial m) (m, (x, y))

    modify $ \bs -> bs { pending = update (pending bs) }

returnLike :: Message -> Bool
returnLike (MethodReturn {}) = True
returnLike (Error {})        = True
returnLike _                 = False

findCorrespondingCall :: Message -> Bustle (Maybe (Message, (Double, Double)))
findCorrespondingCall mr | returnLike mr = do
    let key = (destination mr, inReplyTo mr)
    ps <- gets pending
    case Map.lookup key ps of
        Nothing  -> return Nothing
        Just mcc -> do modify (\bs -> bs { pending = Map.delete key ps })
                       return $ Just mcc
findCorrespondingCall _ = return Nothing


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

abbreviateBusName :: BusName -> BusName
abbreviateBusName n@(':':_) = n
abbreviateBusName n = reverse . takeWhile (/= '.') . reverse $ n

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
memberName m = do
    current <- gets row
    let p = prettyPath $ path m
        meth = abbreviate $ iface m ++ "." ++ member m

    shape $ MemberLabel p meth current

relativeTimestamp :: Message -> Bustle ()
relativeTimestamp m = do
    base <- gets startTime
    let relative = (timestamp m - base) `div` 1000
    current <- gets row
    shape $ Timestamp (show relative ++ "ms") current

returnArc :: Message -> Double -> Double -> Bustle ()
returnArc mr callx cally = do
    destinationx <- destinationCoordinate mr
    currentx     <- senderCoordinate mr
    currenty     <- gets row

    shape $ Arc { topx = callx, topy = cally
                , bottomx = currentx, bottomy = currenty
                , arcside = if (destinationx > currentx) then L else R
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

        MethodReturn {} -> do
            call <- findCorrespondingCall m
            case call of
                Nothing         -> return ()
                Just (_, (x,y)) -> do
                    advance
                    relativeTimestamp m
                    methodReturn m
                    returnArc m x y

        Error {} -> do
            call <- findCorrespondingCall m
            case call of
                Nothing         -> return ()
                Just (_, (x,y)) -> do
                    advance
                    relativeTimestamp m
                    errorReturn m
                    returnArc m x y

  where advance = advanceBy 30 -- FIXME: use some function of timestamp


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
