{-
Bustle.Diagram: turn a list of messages into a set of abstract shapes
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
module Bustle.Diagram
  ( Shape(..)
  , Arrowhead(..)
  , Side(..)
  , Colour(..)
  , Rect
  , bounds
  , draw
  , drawBoundingBox
  , intersects
  )
where

import Control.Arrow ((&&&))
import Control.Applicative ((<$>), (<*>))

import Graphics.Rendering.Cairo

type Point = (Double, Double)
type Rect = (Double, Double, Double, Double)

data Arrowhead = Above | Below
  deriving (Eq, Show, Read, Ord)

above, below :: Arrowhead -> Bool
above Above = True
above Below = False
below = not . above

voffset :: Num a => Arrowhead -> (a -> a -> a)
voffset Above = (-)
voffset Below = (+)

data Side = L | R
  deriving (Eq, Show, Read, Ord)

offset :: Num a => Side -> (a -> a -> a)
offset L = (-)
offset R = (+)

data Colour = Colour Double Double Double
  deriving (Eq, Show, Read, Ord)

data Shape = Header { str :: String, shapex, shapey :: Double
                    }
           | MemberLabel String String Double
           | Timestamp { str :: String, shapey :: Double }
           | ClientLine { shapex, shapey1, shapey2 :: Double }
           | Rule { shapex, shapey :: Double }
           | Arrow { shapecolour :: Maybe Colour
                   , arrowhead :: Arrowhead
                   , shapex1, shapex2, shapey :: Double
                   }
           | SignalArrow { shapex1, epicentre, shapex2, shapey :: Double }
           | Arc { topx, topy, bottomx, bottomy :: Double
                 , arcside :: Side
                 }
  deriving (Show, Read, Eq)

arcControlPoints :: Shape -> (Point, Point)
arcControlPoints (Arc { topx=x1, topy=y1, bottomx=x2, bottomy=y2, arcside=s }) =
    let (+-) = offset s
        cp1 = (x1 +- 60, y1 + 10)
        cp2 = (x2 +- 60, y2 - 10)
    in (cp1, cp2)
arcControlPoints _ = error "i see you've played arcy-shapey before"


--
-- Constants
--
eventHeight :: Double
eventHeight = 30

timestampx, timestampWidth :: Double
timestampx = 30
timestampWidth = 60

memberx, memberWidth :: Double
memberx = 230
memberWidth = 340

columnWidth :: Double
columnWidth = 70


--
-- Calculating bounds of shapes
--
minMax :: Ord a => (a, a) -> (a, a)
minMax = uncurry min &&& uncurry max

xMinMax :: Shape -> (Double, Double)
xMinMax = minMax . (shapex1 &&& shapex2)

fromCentre :: Double -> Double -> Double -> Rect
fromCentre x y width =
    (x - width / 2, y - height / 2,
     x + width / 2, y + height / 2)
  where height = eventHeight

bounds :: Shape -> Rect
bounds s = case s of
  ClientLine {} -> (shapex s, shapey1 s, shapex s, shapey2 s)
  Rule {} -> (0, shapey s, shapex s, shapey s)
  Arrow {} ->
    let (x1, x2) = xMinMax s
        y1 = shapey s - (if above (arrowhead s) then 5 else 0)
        y2 = shapey s + (if below (arrowhead s) then 5 else 0)
    in (x1, y1, x2, y2)
  SignalArrow {} ->
    let (x1, x2) = xMinMax s
        (y1, y2) = (subtract 5) &&& (+5) $ shapey s
    in (x1, y1, x2, y2)
  Arc { topx=x1, bottomx=x2, topy=y1, bottomy=y2 } ->
    let ((cx, _), (dx, _)) = arcControlPoints s
    in (min x1 cx, y1, max x2 dx, y2)
  Timestamp { shapey=y } -> fromCentre timestampx y timestampWidth
  MemberLabel _ _ y -> fromCentre memberx y memberWidth
  Header { shapex = x, shapey = y} -> fromCentre x y columnWidth


intersects :: Rect -> Rect -> Bool
intersects (x,y,w,z) (x', y', w', z') =
  not $ or [x > w', w < x', y > z', z < y']

--
-- Drawing
--
drawBoundingBox :: Shape -> Render ()
drawBoundingBox s = do
    let (x,y,w,z) = bounds s
    save
    setSourceRGB 0 0 1
    rectangle x y (w - x) (z - y)
    stroke
    restore

draw :: Shape -> Render ()
draw s = draw' s
  where draw' = case s of
          Arc {} -> let ((cx, cy), (dx, dy)) = arcControlPoints s
                    in drawArc cx cy dx dy <$>
                          topx <*> topy <*> bottomx <*> bottomy
          SignalArrow {} -> drawSignalArrow <$> epicentre <*> shapex1 <*>
                              shapex2 <*> shapey
          Arrow {} -> drawArrow <$> shapecolour <*> arrowhead <*> shapex1 <*>
                        shapex2 <*> shapey
          Header {} -> drawHeader <$> str <*> shapex <*> shapey
          MemberLabel s1 s2 y -> const (drawMember s1 s2 y)
          Timestamp {} -> drawTimestamp <$> str <*> shapey
          ClientLine {} -> drawClientLine <$> shapex <*> shapey1 <*> shapey2
          Rule {} -> drawRule <$> shapex <*> shapey


halfArrowHead :: Arrowhead -> Bool -> Render ()
halfArrowHead a left = do
    (x,y) <- getCurrentPoint
    let x' = if left then x - 10 else x + 10
    let y' = voffset a y 5
    if left -- work around weird artifacts
      then moveTo x' y' >> lineTo x y
      else lineTo x' y' >> moveTo x y

arrowHead :: Bool -> Render ()
arrowHead left = halfArrowHead Above left >> halfArrowHead Below left

drawArrow :: Maybe Colour -> Arrowhead -> Double -> Double -> Double
          -> Render ()
drawArrow c a from to y = do
    save

    case c of Nothing -> return ()
              Just (Colour r g b) -> setSourceRGB r g b

    moveTo from y
    lineTo to y
    halfArrowHead a (from < to)
    stroke

    restore

drawSignalArrow :: Double -> Double -> Double -> Double -> Render ()
drawSignalArrow e left right y = do
    newPath
    arc e y 5 0 (2 * pi)
    stroke

    moveTo (left - 20) y
    arrowHead False
    lineTo (e - 5) y
    stroke

    moveTo (e + 5) y
    lineTo (right + 20) y
    arrowHead True
    stroke

drawArc :: Double -> Double -> Double -> Double
        -> Double -> Double -> Double -> Double
        -> Render ()
drawArc cx cy dx dy x1 y1 x2 y2 = do
    save

    setSourceRGB 0.4 0.7 0.4
    setDash [3, 3] 0

    moveTo x1 y1
    curveTo cx cy dx dy x2 y2
    stroke

    restore

drawHeader :: String -> Double -> Double -> Render ()
drawHeader name x y = do
    extents <- textExtents name
    let diff = textExtentsWidth extents / 2
    moveTo (x - diff) (y + 10)
    showText name

drawMember :: String -> String -> Double -> Render ()
drawMember s1 s2 y = do
    -- TODO: Make this fit in the bounds we estimated
    moveTo timestampWidth y
    showText s1
    moveTo timestampWidth (y + 10)
    showText s2

drawTimestamp :: String -> Double -> Render ()
drawTimestamp ts y = do
    moveTo 0 y
    showText ts

drawClientLine :: Double -> Double -> Double -> Render ()
drawClientLine x y1 y2 = do
    save

    setSourceRGB 0.7 0.7 0.7
    moveTo x y1
    lineTo x y2
    stroke

    restore

drawRule :: Double -> Double -> Render ()
drawRule x y = do
    save

    setSourceRGB 0.7 0.4 0.4
    setLineWidth 0.2

    moveTo 0 y
    lineTo x y
    stroke

    restore

-- vim: sw=2 sts=2
