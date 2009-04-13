{-
Bustle.Diagram: shapes for sequence diagrams
Copyright (C) 2008–2009 Collabora Ltd.

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
  , Diagram
  , Arrowhead(..)
  , Side(..)
  , Colour(..)
  , Rect
  , dimensions
  , drawDiagram
  , drawRegion
  , headers
  )
where

import Data.Maybe (maybe)
import Control.Arrow ((&&&), (***))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)

import Control.Monad.Reader

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo (cairoCreateContext, showLayout)
import Graphics.UI.Gtk.Pango.Layout
import Graphics.UI.Gtk.Pango.Font

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

data Shape = Header { strs :: [String]
                    , shapex, shapey :: Double
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
                 , caption :: String
                 }
  deriving (Show, Read, Eq)

type Diagram = [Shape]

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
  Header { strs = ss, shapex = x, shapey = y} ->
    let n = fromIntegral $ length ss
        width = columnWidth
        height = 10 * n
    in (x - width / 2, y,
        x + width / 2, y + height)

intersects :: Rect -> Rect -> Bool
intersects (x,y,w,z) (x', y', w', z') =
  not $ or [x > w', w < x', y > z', z < y']

headers :: [(Double, [String])] -> Double -> (Double, [Shape])
headers []  _ = (0, [])
headers xss y = (bottomLine - y, botAligned)
  where topAligned = map (\(x, ss) -> Header ss x y) xss
        bottoms    = map (frth4 . bounds) topAligned
        bottomLine = maximum bottoms
        adjs       = map (bottomLine -) bottoms
        botAligned = map (\(h, adj) -> h { shapey = shapey h + adj })
                         (zip topAligned adjs)
        frth4 (_,_,_,y2) = y2

--
-- Drawing
--

dimensions :: Diagram -> (Double, Double)
dimensions shapes = (maximum . (0:) *** maximum . (0:)) xys
  where xys = unzip [ (x2, y2) | (_, _, x2, y2) <- map bounds shapes ]

drawDiagram :: Bool -> Diagram -> Render ()
drawDiagram drawBounds shapes = do
    clearCanvas
    forM_ shapes $ \x -> do
        when drawBounds (drawBoundingBox x)
        draw x

drawRegion :: Rect -> Bool -> Diagram -> Render ()
drawRegion r db = drawDiagram db . visible r . map (bounds &&& id)

visible :: Rect -> [(Rect, Shape)] -> [Shape]
visible r = map snd . filter (intersects r . fst)


saved :: Render () -> Render ()
saved act = save >> act >> restore

clearCanvas :: Render ()
clearCanvas = saved $ do
    setSourceRGB 1 1 1
    setOperator OperatorSource
    paint

drawBoundingBox :: Shape -> Render ()
drawBoundingBox s = saved $ do
    let (x,y,w,z) = bounds s
    setSourceRGB 0 0 1
    rectangle x y (w - x) (z - y)
    stroke

draw :: Shape -> Render ()
draw s = draw' s
  where draw' = case s of
          Arc {} -> let ((cx, cy), (dx, dy)) = arcControlPoints s
                    in drawArc cx cy dx dy <$>
                          topx <*> topy <*> bottomx <*> bottomy <*> caption
          SignalArrow {} -> drawSignalArrow <$> epicentre <*> shapex1 <*>
                              shapex2 <*> shapey
          Arrow {} -> drawArrow <$> shapecolour <*> arrowhead <*> shapex1 <*>
                        shapex2 <*> shapey
          Header {} -> drawHeader <$> strs <*> shapex <*> shapey
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
drawArrow c a from to y = saved $ do
    maybe (return ()) (\(Colour r g b) -> setSourceRGB r g b) c

    moveTo from y
    lineTo to y
    halfArrowHead a (from < to)
    stroke

drawSignalArrow :: Double -> Double -> Double -> Double -> Render ()
drawSignalArrow e left right y = do
    newPath
    arc e y 5 0 (2 * pi)
    stroke

    moveTo left y
    arrowHead False
    lineTo (e - 5) y
    stroke

    moveTo (e + 5) y
    lineTo right y
    arrowHead True
    stroke

drawArc :: Double -> Double -> Double -> Double
        -> Double -> Double -> Double -> Double
        -> String
        -> Render ()
drawArc cx cy dx dy x1 y1 x2 y2 cap = do
    save

    setSourceRGB 0.4 0.7 0.4
    setDash [3, 3] 0

    moveTo x1 y1
    curveTo cx cy dx dy x2 y2
    stroke

    setSourceRGB 0 0 0
    extents <- textExtents cap
    let textWidth = textExtentsWidth extents
        tx = min x2 dx + abs (x2 - dx) / 2
    moveTo (if x1 > cx then tx - textWidth else tx) y2
    showText cap

{-
    -- bounding box of arc; useful as a guide to the coordinates we have to
    -- play with.
    setSourceRGB 0 0 1
    rectangle x1 y1 (cx - x1) (y2 - y1)
    stroke
-}

    restore

font :: IO FontDescription
font = do
    fd <- fontDescriptionNew
    fontDescriptionSetSize fd 7
    fontDescriptionSetFamily fd "Sans"
    return fd

mkLayout :: (MonadIO m)
         => String -> EllipsizeMode -> LayoutAlignment -> Double
         -> m PangoLayout
mkLayout s e a w = liftIO $ do
    ctx <- cairoCreateContext Nothing
    layout <- layoutEmpty ctx
    layoutSetMarkup layout s
    layoutSetFontDescription layout . Just =<< font
    layoutSetEllipsize layout e
    layoutSetAlignment layout a
    layoutSetWidth layout (Just w)
    return layout

drawHeader :: [String] -> Double -> Double -> Render ()
drawHeader names x y = forM_ (zip [0..] names) $ \(i, name) -> do
    l <- mkLayout name EllipsizeEnd AlignCenter columnWidth
    moveTo (x - (columnWidth / 2)) (y + i * h)
    showLayout l
  where h = 10

drawMember :: String -> String -> Double -> Render ()
drawMember s1 s2 y = dm s1 (y - 10) >> dm s2 y
  where
    dm s y' = do
      l <- mkLayout s EllipsizeStart AlignLeft memberWidth
      moveTo timestampWidth y'
      showLayout l

drawTimestamp :: String -> Double -> Render ()
drawTimestamp ts y = do
    moveTo 0 (y - 10)
    showLayout =<< mkLayout ts EllipsizeNone AlignLeft timestampWidth

drawClientLine :: Double -> Double -> Double -> Render ()
drawClientLine x y1 y2 = saved $ do
    setSourceRGB 0.7 0.7 0.7
    moveTo x y1
    lineTo x y2
    stroke

drawRule :: Double -> Double -> Render ()
drawRule x y = saved $ do
    setSourceRGB 0.7 0.1 0.1
    setLineWidth 0.2

    moveTo 0 y
    lineTo x y
    stroke

-- vim: sw=2 sts=2
