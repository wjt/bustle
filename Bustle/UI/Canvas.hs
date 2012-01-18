{-
Bustle.UI.Canvas: displays diagrams
Copyright © 2008–2012 Collabora Ltd.

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
module Bustle.UI.Canvas
  (
    Canvas
  , canvasNew

  , canvasGetShapes
  , canvasSetShapes

  , canvasFocus
  , canvasScrollToBottom
  )
where

import Data.Maybe (isNothing)
import Data.IORef
import Control.Monad (when)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import Bustle.Diagram
import Bustle.Regions
import Bustle.Util

data Canvas a =
    Canvas { canvasLayout :: Layout
           , canvasClampIdleId :: IORef (Maybe HandlerId)

           , canvasShapes :: IORef Diagram
           , canvasWidth :: IORef Double

           , canvasSelection :: IORef (RegionSelection a)
           , canvasSelectionChangedCb :: Maybe a -> IO ()

           , canvasShowBounds :: Bool
           }

canvasNew :: Eq a
          => GladeXML
          -> Bool
          -> (Maybe a -> IO ())
          -> IO (Canvas a)
canvasNew xml showBounds selectionChangedCb = do
    layout <- xmlGetWidget xml castToLayout "diagramLayout"
    idRef <- newIORef Nothing
    shapesRef <- newIORef []
    widthRef <- newIORef 0
    rsRef <- newIORef $ regionSelectionNew []

    let canvas = Canvas layout idRef shapesRef widthRef rsRef selectionChangedCb showBounds
    setupCanvas canvas
    return canvas

-- Add/remove one step/page increment from an Adjustment, limited to the top of
-- the last page.
incStep, decStep, incPage{-, decPage -} :: Adjustment -> IO ()
incStep = incdec (+) adjustmentGetStepIncrement
decStep = incdec (-) adjustmentGetStepIncrement
incPage = incdec (+) adjustmentGetPageIncrement
--decPage = incdec (-) adjustmentGetPageIncrement

incdec :: (Double -> Double -> Double) -- How to combine the increment
       -> (Adjustment -> IO Double)    -- Action to discover the increment
       -> Adjustment
       -> IO ()
incdec (+-) f adj = do
    pos <- adjustmentGetValue adj
    step <- f adj
    page <- adjustmentGetPageSize adj
    lim <- adjustmentGetUpper adj
    adjustmentSetValue adj $ min (pos +- step) (lim - page)

setupCanvas :: Eq a
            => Canvas a
            -> IO ()
setupCanvas canvas = do
    let layout = canvasLayout canvas

    -- Scrolling
    hadj <- layoutGetHAdjustment layout
    vadj <- layoutGetVAdjustment layout

    adjustmentSetStepIncrement hadj eventHeight
    adjustmentSetStepIncrement vadj eventHeight

    layout `on` keyPressEvent $ tryEvent $ do
      [] <- eventModifier
      key <- eventKeyName
      case key of
        "Left"      -> io $ decStep hadj
        "Right"     -> io $ incStep hadj
        "space"     -> io $ incPage vadj
        _           -> stopEvent

    let updateWith f = io $ canvasUpdateSelection canvas f

    -- Clicking
    layout `on` buttonPressEvent $ tryEvent $ do
      io $ layout `set` [ widgetIsFocus := True ]
      LeftButton <- eventButton
      (_, y) <- eventCoordinates

      updateWith (regionSelectionUpdate y)

    -- Keyboard navigation
    layout `on` keyPressEvent $ tryEvent $ do
      [] <- eventModifier
      key <- eventKeyName
      case key of
        "Up"        -> updateWith regionSelectionUp
        "Down"      -> updateWith regionSelectionDown
        "Home"      -> updateWith regionSelectionFirst
        "End"       -> updateWith regionSelectionLast
        _           -> stopEvent

    -- Expose events
    -- I think we could speed things up by only showing the revealed area
    -- rather than everything that's visible.
    layout `on` exposeEvent $ tryEvent $ io $ canvasUpdate canvas

    return ()

canvasInvalidateArea :: Canvas a
                     -> Int
                     -> Int
                     -> Int
                     -> Int
                     -> IO ()
canvasInvalidateArea canvas x1 y1 x2 y2 = do
    let layout = canvasLayout canvas
    realized <- widgetGetRealized layout

    when realized $ do
        win <- layoutGetDrawWindow layout
        let pangoRectangle = Rectangle x1 y1 x2 y2
        drawWindowInvalidateRect win pangoRectangle False

canvasInvalidateStripe :: Canvas a
                       -> Stripe
                       -> IO ()
canvasInvalidateStripe canvas (Stripe y1 y2) = do
    let layout = canvasLayout canvas
    realized <- widgetGetRealized layout

    -- We only need to invalidate ourself if we're actually on the screen
    when realized $ do
        win <- layoutGetDrawWindow layout
        (width, _height) <- layoutGetSize layout
        let pangoRectangle = Rectangle 0 (floor y1) width (ceiling y2)

        drawWindowInvalidateRect win pangoRectangle False

canvasClampAroundSelection :: Canvas a
                           -> IO ()
canvasClampAroundSelection canvas = do
    let idRef = canvasClampIdleId canvas

    id_ <- readIORef idRef
    when (isNothing id_) $ do
        id' <- flip idleAdd priorityDefaultIdle $ do
            rs <- readIORef $ canvasSelection canvas
            case rsCurrent rs of
                Nothing -> return ()
                Just (Stripe top bottom, _) -> do
                    vadj <- layoutGetVAdjustment $ canvasLayout canvas
                    let padding = (bottom - top) / 2
                    adjustmentClampPage vadj (top - padding) (bottom + padding)

            writeIORef idRef Nothing
            return False

        writeIORef idRef (Just id')

canvasGetSelection :: Canvas a
                   -> IO (Maybe (Stripe, a))
canvasGetSelection canvas = do
    rs <- readIORef $ canvasSelection canvas

    return $ rsCurrent rs

canvasUpdateSelection :: Eq a
                      => Canvas a
                      -> (RegionSelection a -> RegionSelection a)
                      -> IO ()
canvasUpdateSelection canvas f = do
    let regionSelectionRef = canvasSelection canvas
    rs <- readIORef regionSelectionRef
    let currentMessage = rsCurrent rs
        rs' = f rs
        newMessage = rsCurrent rs'
    writeIORef regionSelectionRef rs'

    when (newMessage /= currentMessage) $ do
        maybeM currentMessage $ \(r, _) ->
            canvasInvalidateStripe canvas r

        maybeM newMessage $ \(r, _) -> do
            canvasInvalidateStripe canvas r
            canvasClampAroundSelection canvas

        canvasSelectionChangedCb canvas (fmap snd newMessage)

canvasSetShapes :: Eq a
                => Canvas a
                -> Diagram
                -> Regions a
                -> Double -- Yuck. These shouldn't be here.
                -> Int    -- No no no!
                -> IO ()
canvasSetShapes canvas shapes regions centreOffset windowWidth = do
    let (width, height) = diagramDimensions shapes
        layout = canvasLayout canvas

    writeIORef (canvasShapes canvas) shapes
    writeIORef (canvasWidth canvas) width

    canvasUpdateSelection canvas $ \rs ->
      let
        rs' = regionSelectionNew regions
      in
        case rsCurrent rs of
            Just (_, x) -> regionSelectionSelect x rs'
            Nothing     -> rs'

    layoutSetSize layout (floor width) (floor height)
    canvasInvalidateArea canvas 0 0 (floor width) (floor height)

    -- FIXME: only do this the first time maybe?
    -- Shift to make the timestamp column visible
    hadj <- layoutGetHAdjustment layout
    -- Roughly centre the timestamp-and-member column
    adjustmentSetValue hadj
        (centreOffset -
            (fromIntegral windowWidth - timestampAndMemberWidth) / 2
        )

canvasGetShapes :: Canvas a
                -> IO Diagram
canvasGetShapes = readIORef . canvasShapes

-- | Redraws the currently-visible area of the canvas
canvasUpdate :: Canvas a
             -> IO ()
canvasUpdate canvas = do
    current <- canvasGetSelection canvas
    shapes <- canvasGetShapes canvas
    width <- readIORef $ canvasWidth canvas
    let shapes' = case current of
            Nothing     -> shapes
            Just (Stripe y1 y2, _) -> Highlight (0, y1, width, y2):shapes

    let layout = canvasLayout canvas

    hadj <- layoutGetHAdjustment layout
    hpos <- adjustmentGetValue hadj
    hpage <- adjustmentGetPageSize hadj

    vadj <- layoutGetVAdjustment layout
    vpos <- adjustmentGetValue vadj
    vpage <- adjustmentGetPageSize vadj

    let r = (hpos, vpos, hpos + hpage, vpos + vpage)

    win <- layoutGetDrawWindow layout
    renderWithDrawable win $ drawRegion r (canvasShowBounds canvas) shapes'

canvasFocus :: Canvas a
            -> IO ()
canvasFocus canvas = do
    (canvasLayout canvas) `set` [ widgetIsFocus := True ]

canvasScrollToBottom :: Canvas a
                     -> IO ()
canvasScrollToBottom canvas = do
    vadj <- layoutGetVAdjustment (canvasLayout canvas)
    page <- adjustmentGetPageSize vadj
    lim <- adjustmentGetUpper vadj
    adjustmentSetValue vadj (max 0 (lim - page))
