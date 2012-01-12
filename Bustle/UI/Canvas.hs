module Bustle.UI.Canvas
  (
    Canvas
  , canvasNew

  , canvasLayout

  , canvasInvalidateStripe
  , canvasClampAroundSelection
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

data Canvas =
    Canvas { canvasLayout :: Layout
           , canvasClampIdleId :: IORef (Maybe HandlerId)
           }

canvasNew :: GladeXML
          -> IO Canvas
canvasNew xml = do
    layout <- xmlGetWidget xml castToLayout "diagramLayout"

    setupPanning layout

    idRef <- newIORef Nothing
    return $ Canvas layout idRef

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

setupPanning :: Layout
             -> IO ()
setupPanning layout = do
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

    return ()

canvasInvalidateStripe :: Canvas
                       -> Stripe
                       -> IO ()
canvasInvalidateStripe canvas (Stripe y1 y2) = do
    let layout = canvasLayout canvas
    win <- layoutGetDrawWindow layout
    (width, _height) <- layoutGetSize layout
    let pangoRectangle = Rectangle 0 (floor y1) width (ceiling y2)

    drawWindowInvalidateRect win pangoRectangle False

canvasClampAroundSelection :: Canvas
                           -> IORef (RegionSelection a)
                           -> IO ()
canvasClampAroundSelection canvas regionSelectionRef = do
    let idRef = canvasClampIdleId canvas
    id_ <- readIORef idRef
    when (isNothing id_) $ do
        id' <- flip idleAdd priorityDefaultIdle $ do
            rs <- readIORef regionSelectionRef
            case rsCurrent rs of
                Nothing -> return ()
                Just (Stripe top bottom, _) -> do
                    vadj <- layoutGetVAdjustment $ canvasLayout canvas
                    let padding = (bottom - top) / 2
                    adjustmentClampPage vadj (top - padding) (bottom + padding)

            writeIORef idRef Nothing
            return False

        writeIORef idRef (Just id')
