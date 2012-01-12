module Bustle.UI.Canvas
  (
    Canvas
  , canvasNew

  -- FIXME: move the stuff that needs this into this file.
  , canvasLayout

  , canvasGetSelection
  , canvasUpdateSelection
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

           , canvasSelection :: IORef (RegionSelection a)
           , canvasSelectionChangedCb :: Maybe a -> IO ()
           }

canvasNew :: Eq a
          => GladeXML
          -> (Maybe a -> IO ())
          -> IO (Canvas a)
canvasNew xml selectionChangedCb = do
    layout <- xmlGetWidget xml castToLayout "diagramLayout"
    idRef <- newIORef Nothing
    rsRef <- newIORef $ regionSelectionNew []

    let canvas = Canvas layout idRef rsRef selectionChangedCb
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

    return ()

canvasInvalidateStripe :: Canvas a
                       -> Stripe
                       -> IO ()
canvasInvalidateStripe canvas (Stripe y1 y2) = do
    let layout = canvasLayout canvas
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
