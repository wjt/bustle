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


process :: [Message] -> Render ()
process log = evalStateT (mapM_ munge (filter relevant log)) bs
    where bs = BustleState Map.empty Map.empty 0 0
          relevant (MethodReturn {}) = True
          relevant (Error        {}) = True
          relevant m                 = path m /= "/org/freedesktop/DBus"


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

findCorrespondingCall mr@(MethodReturn {}) = do
    let key = (destination mr, inReplyTo mr)
    ps <- gets pending
    case Map.lookup key ps of
        Nothing  -> return Nothing
        Just mcc -> do modify (\bs -> bs { pending = Map.delete key ps })
                       return $ Just mcc
findCorrespondingCall _ = return Nothing


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


returnArc mr callx cally = do
    destinationx <- destinationCoordinate mr
    currentx     <- senderCoordinate mr
    currenty     <- gets row

    lift $ dottyArc (destinationx > currentx) currentx currenty callx cally

munge :: Message -> StateT BustleState Render ()
munge m = case m of
        Signal {}       -> do
            advance
            memberName m
            signal m

        MethodCall {}   -> do
            advance
            memberName m
            methodCall m
            addPending m

        MethodReturn {} -> do
            call <- findCorrespondingCall m
            case call of
                Nothing         -> return ()
                Just (_, (x,y)) -> do
                    advance
                    methodReturn m
                    returnArc m x y

        Error {}        -> error "eh"
  where advance = advanceBy 30 -- FIXME: use some function of timestamp


methodCall = methodLike True
methodReturn = methodLike False

methodLike above m = do
    sc <- senderCoordinate m
    dc <- destinationCoordinate m
    t <- gets row
    lift $ halfArrow above sc dc t

signal m = do
    x <- senderCoordinate m
    t <- gets row
    cs <- gets coordinates
    let (left, right) = (Map.fold min 10000 cs, Map.fold max 0 cs)
    lift $ signalArrow x left right t


--
-- Shapes
--

halfArrowHead :: Bool -> Bool -> Render ()
halfArrowHead above left = do
    (x,y) <- getCurrentPoint
    let x' = if left then x - 10 else x + 10
    let y' = if above then y - 5 else y + 5
    if left -- work around weird artifacts
      then moveTo x' y' >> lineTo x y
      else lineTo x' y' >> moveTo x y

arrowHead :: Bool -> Render ()
arrowHead left = halfArrowHead False left >> halfArrowHead True left

halfArrow :: Bool -> Double -> Double -> Double -> Render ()
halfArrow above from to y = do
    moveTo from y
    lineTo to y
    halfArrowHead above (from < to)
    stroke

signalArrow :: Double -> Double -> Double -> Double -> Render ()
signalArrow epicentre left right y = do
    newPath
    arc epicentre y 5 0 (2 * pi)
    stroke

    moveTo (left - 20) y
    arrowHead False
    lineTo (epicentre - 5) y
    stroke

    moveTo (epicentre + 5) y
    lineTo (right + 20) y
    arrowHead True
    stroke


dottyArc :: Bool -> Double -> Double -> Double -> Double -> Render ()
dottyArc left startx starty endx endy = do
    let offset = if left then (-) else (+)

    setSourceRGB 0.4 0.7 0.4
    setDash [3, 3] 0

    moveTo startx starty
    curveTo (startx `offset` 60) (starty - 10)
            (endx   `offset` 60) (endy   + 10)
            endx endy
    stroke

    setSourceRGB 0 0 0
    setDash [] 0

-- vim: sw=2 sts=2
