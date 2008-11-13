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
  , bounds
  )
where

import Bustle.Types

import Control.Arrow ((&&&))

-- :'(
import System.IO.Unsafe (unsafePerformIO)
import Graphics.Rendering.Cairo

type Point = (Double, Double)
type Rect = (Double, Double, Double, Double)

data Arrowhead = Above | Below | Both
  deriving (Eq, Show, Read, Ord)

above :: Arrowhead -> Bool
above Below = False
above _ = True

data Side = L | R
  deriving (Eq, Show, Read, Ord)

offset :: Num a => Side -> (a -> a -> a)
offset L = (-)
offset R = (+)

data Colour = Colour Double Double Double
  deriving (Eq, Show, Read, Ord)

data Shape = Header { str :: String
                    , shapex :: Double
                    , shapey :: Double
                    }
           | MemberName ObjectPath Interface BusName Double
           | Timestamp { str :: String, shapey :: Double }
           | ClientLine { shapex :: Double
                        , shapey1 :: Double
                        , shapey2 :: Double
                        }
           | Arrow { shapecolour :: Colour
                   , arrowhead :: Arrowhead
                   , shapex1 :: Double
                   , shapex2 :: Double
                   , shapey  :: Double
                   }
           | SignalArrow { shapex1 :: Double
                         , epicentre :: Double
                         , shapex2 :: Double
                         , shapey :: Double
                         }
           | Arc { topx :: Double
                 , topy :: Double
                 , bottomx :: Double
                 , bottomy :: Double
                 , side :: Side
                 }
  deriving (Show, Read, Eq)


minMax :: Ord a => (a, a) -> (a, a)
minMax = uncurry min &&& uncurry max

xMinMax :: Shape -> (Double, Double)
xMinMax = minMax . (shapex1 &&& shapex2)

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

fromCentre :: Double -> Double -> Double -> Rect
fromCentre x y width =
    (x - width / 2, y - height / 2,
     x + width / 2, y + height / 2)
  where height = eventHeight

bounds :: Shape -> Rect
bounds s = case s of
  ClientLine {} -> (shapex s, shapey1 s, shapex s, shapey2 s)
  Arrow {} ->
    let diff = (if above (arrowhead s) then 5 else 0)
        (x1, x2) = xMinMax s
        y1 = shapey s - diff
        y2 = shapey s + diff
    in (x1, y1, x2, y2)
  SignalArrow {} ->
    let (x1, x2) = xMinMax s
        (y1, y2) = (subtract 5) &&& (+5) $ shapey s
    in (x1, y1, x2, y2)
  Arc { topx=x1, bottomx=x2, topy=y1, bottomy=y2 } ->
    let ((cx, _), (dx, _)) = arcControlPoints s
    in (min x1 cx, y1, max x2 dx, y2)
  Timestamp { shapey=y } -> fromCentre timestampx y timestampWidth
  MemberName _ _ _ y -> fromCentre memberx y memberWidth
  Header { shapex = x, shapey = y} -> fromCentre x y columnWidth

arcControlPoints :: Shape -> (Point, Point)
arcControlPoints (Arc { topx=x1, topy=y1, bottomx=x2, bottomy=y2, side=s }) =
    let (+-) = offset s
        cp1 = (x1 +- 60, y1 + 10)
        cp2 = (x2 +- 60, y2 - 10)
    in (cp1, cp2)
arcControlPoints _ = error "i see you've played arcy-shapey before"

-- vim: sw=2 sts=2
