{-# LANGUAGE RecordPuns #-}
module Main where

import Bustle.Types
import Bustle.Parser

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad.State
import Control.Monad (forM_)

import Graphics.UI.Gtk hiding (get)
import Graphics.Rendering.Cairo
import Control.Monad.Trans ( liftIO )

main = do
    input <- readFile "/tmp/face"
    let Right log = readLog input
    run $ process log

{-
Plan:
    State is ( application => x-coordinate
             , (application, serial) => (Message, y-coordinate of the call)
             , current position
             )
    * Generate a bunch of Render actions
    * When encountering a Return, add a curve back to where it came from and
      remove it from state.
    * Could optimize the position of applications if we wanted
-}

data BustleState =
    BustleState { coordinates :: Map BusName Double
                , pending :: Map (BusName, Serial) (Message, Double)
                , row :: Double
                }

modifyRow f = modify (\bs -> bs { row = f (row bs)})
modifyCoordinates f = modify (\bs -> bs { coordinates = f (coordinates bs)})

lastComponent :: BusName -> String
lastComponent = reverse . takeWhile (/= '.') . reverse

addApplication :: BusName -> Double -> StateT BustleState Render Double
addApplication s c = do
    let name = lastComponent s
    lift $ do extents <- textExtents name
              let diff = textExtentsWidth extents / 2
              moveTo (c - diff) 10
              showText name
    modifyCoordinates (Map.insert s c)
    return c

appCoordinate :: BusName -> StateT BustleState Render Double
appCoordinate s = do
    cs <- gets coordinates
    case Map.lookup s cs of
        Just c  -> return c
        Nothing -> do let c = Map.fold max 0 cs + 40
                      addApplication s c

senderCoordinate :: Message -> StateT BustleState Render Double
senderCoordinate m = appCoordinate (sender m)

destinationCoordinate :: Message -> StateT BustleState Render Double
destinationCoordinate m = appCoordinate (destination m)

munge :: Message -> StateT BustleState Render ()
munge m = do
    modifyRow (+30) -- FIXME: use some function of timestamp
    sc <- senderCoordinate m
    case m of
        Signal {} -> signal sc
        MethodCall {}   -> destinationCoordinate m >>= methodCall sc
        MethodReturn {} -> destinationCoordinate m >>= methodReturn sc
        Error {}        -> error "eh"

drawApplicationGuides :: StateT BustleState Render ()
drawApplicationGuides = do
    lift $ do setSourceRGB 0.7 0.7 0.7
              setLineWidth 1
    xs <- gets (Map.fold (:) [] . coordinates)
    y <- gets row
    forM_ xs $ \x -> lift $ do
        moveTo x 15
        lineTo x y
        stroke

process' :: [Message] -> StateT BustleState Render ()
process' log = do
    mapM_ munge log
    drawApplicationGuides

process :: [Message] -> Render ()
process log = evalStateT (process' log) bs
    where bs = BustleState Map.empty Map.empty 10

halfArrow above x x' = do
    t <- gets row
    lift $ do
        moveTo x t
        lineTo x' t
        let arrowHeadX = if x < x' then x' - 10 else x' + 10
        let arrowHeadY = if above then t - 5 else t + 5
        lineTo arrowHeadX arrowHeadY
        stroke

methodCall = halfArrow True
methodReturn = halfArrow False

signal x = do
    t <- gets row
    lift $ do
        moveTo (x - 20) t
        lineTo (x + 20) t
--        let arrowHeadX = if x < x' then x' - 10 else x' + 10
--        mapM_ (\y -> lineTo arrowHeadX y >> moveTo x' t) [t-5, t+5]
        stroke

run :: Render () -> IO ()
run act = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockClose ResponseClose
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  canvas `onSizeRequest` return (Requisition 250 250)
  canvas `onExpose` updateCanvas canvas act
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  widgetDestroy dia
  -- Flush all commands that are waiting to be sent to the graphics server.
  -- This ensures that the window is actually closed before ghci displays the
  -- prompt again.
  flush

  where updateCanvas :: DrawingArea -> Render () -> Event -> IO Bool
        updateCanvas canvas act (Expose {}) = do
          win <- widgetGetDrawWindow canvas
          renderWithDrawable win act
          return True
        updateCanvas canvas act _ = return False

-- vim: sw=2 sts=2
