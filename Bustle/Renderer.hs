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

import Bustle.Types

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad.State
import Control.Monad (forM_)

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)

import Graphics.Rendering.Cairo

data BustleState =
    BustleState { coordinates :: Map BusName Double
                , pending :: Map (BusName, Serial) (Message, (Double, Double))
                , row :: Double
                , mostRecentLabels :: Double
                }

modifyCoordinates f = modify (\bs -> bs { coordinates = f (coordinates bs)})

addPending m = do
    x <- destinationCoordinate m
    y <- gets row
    let update = Map.insert (sender m, serial m) (m, (x, y))

    modify $ \bs -> bs { pending = update (pending bs) }

advanceBy :: Double -> StateT BustleState Render ()
advanceBy d = do
    lastLabelling <- gets mostRecentLabels

    current <- gets row

    when (current - lastLabelling > 400) $ do
        xs <- gets (Map.toList . coordinates)
        forM_ xs $ \(name, x) -> lift $ do
            drawHeader name x (current + d)
        modify $ \bs -> bs { mostRecentLabels = (current + d)
                           , row = row bs + d
                           }

    current <- gets row
    modify (\bs -> bs { row = row bs + d })
    next <- gets row

    lift $ do setSourceRGB 0.7 0.7 0.7
              setLineWidth 1

    xs <- gets (Map.fold (:) [] . coordinates)
    forM_ xs $ \x -> lift $ do
        moveTo x (current + 15)
        lineTo x (next + 15)
        stroke

    lift $ do setSourceRGB 0 0 0
              setLineWidth 2

drawHeader :: BusName -> Double -> Double -> Render ()
drawHeader name' x y = do
    let name = abbreviate name'
    extents <- textExtents name
    let diff = textExtentsWidth extents / 2
    moveTo (x - diff) (y + 10)
    showText name

addApplication :: BusName -> Double -> StateT BustleState Render Double
addApplication s c = do
    currentRow <- gets row

    lift $ do drawHeader s c (currentRow - 20)

              setSourceRGB 0.7 0.7 0.7
              setLineWidth 1

              moveTo c (currentRow - 5)
              lineTo c (currentRow + 15)
              stroke

              setSourceRGB 0 0 0
              setLineWidth 2

    modifyCoordinates (Map.insert s c)
    return c

firstAppX = 300

appCoordinate :: BusName -> StateT BustleState Render Double
appCoordinate s = do
    cs <- gets coordinates
    case Map.lookup s cs of
        Just c  -> return c
        Nothing -> do let c = Map.fold max firstAppX cs + 70
                      addApplication s c

senderCoordinate :: Message -> StateT BustleState Render Double
senderCoordinate m = appCoordinate (sender m)

destinationCoordinate :: Message -> StateT BustleState Render Double
destinationCoordinate m = appCoordinate (destination m)

abbreviate :: Interface -> Interface
abbreviate i = fromMaybe i $ stripPrefix "org.freedesktop." i

prettyPath :: ObjectPath -> ObjectPath
prettyPath p = fromMaybe p $ stripPrefix "/org/freedesktop/Telepathy/Connection/" p

memberName :: Message -> StateT BustleState Render ()
memberName m = do
    current <- gets row

    lift $ do
        moveTo 0 current
        showText . prettyPath $ path m

        moveTo 0 (current + 10)
        showText . abbreviate $ iface m ++ " . " ++ member m


returnArc mr = do
    ps <- gets pending
    case Map.lookup (destination mr, inReplyTo mr) ps of
        Nothing -> return False
        Just (_, (callx, cally)) -> do
          currentx <- senderCoordinate mr
          currenty <- gets row

          destinationx <- destinationCoordinate mr

          lift $ do
            let returnIsRTL = destinationx < currentx
                offset = if returnIsRTL then (+) else (-)

                curveByReturnX = currentx `offset` 60
                curveByCallX   = callx    `offset` 60

            setSourceRGB 0.4 0.7 0.4
            setDash [3, 3] 0

            moveTo currentx currenty
            curveTo curveByReturnX (currenty - 10)
                    curveByCallX (cally + 10)
                    callx cally
            stroke

            setSourceRGB 0 0 0
            setDash [] 0
            -- XXX Remove from pending
            return True

munge :: Message -> StateT BustleState Render ()
munge m = do
    advanceBy 30 -- FIXME: use some function of timestamp

    case m of
        Signal {}       -> do
            memberName m
            signal m

        MethodCall {}   -> do
            memberName m
            methodCall m
            addPending m

        MethodReturn {} -> do
            found <- returnArc m

            when found $ methodReturn m


        Error {}        -> error "eh"

halfArrowHead above left = do
    (x,y) <- getCurrentPoint
    let x' = if left then x - 10 else x + 10
    let y' = if above then y - 5 else y + 5
    if left -- work around weird artifacts
      then moveTo x' y' >> lineTo x y
      else lineTo x' y' >> moveTo x y

arrowHead left = halfArrowHead False left >> halfArrowHead True left

halfArrow above m = do
    sc <- senderCoordinate m
    dc <- destinationCoordinate m
    t <- gets row
    lift $ do
        moveTo sc t
        lineTo dc t
        halfArrowHead above (sc < dc)
        stroke

methodCall = halfArrow True
methodReturn = halfArrow False

signal m = do
    x <- senderCoordinate m
    t <- gets row
    cs <- gets coordinates
    let (left, right) = (Map.fold min 10000 cs, Map.fold max 0 cs)
    lift $ do
        newPath
        arc x t 5 0 (2 * pi)
        stroke

        moveTo (left - 20) t
        arrowHead False
        lineTo (x - 5) t
        stroke

        moveTo (x + 5) t
        lineTo (right + 20) t
        arrowHead True
        stroke


process' :: [Message] -> StateT BustleState Render ()
process' = mapM_ munge

process :: [Message] -> Render ()
process log = evalStateT (process' (filter relevant log)) bs
    where bs = BustleState Map.empty Map.empty 0 0
          relevant (MethodReturn {}) = True
          relevant (Error        {}) = True
          relevant m                 = path m /= "/org/freedesktop/DBus"
