{-
Bustle: a tool to draw charts of D-Bus activity
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
{-# LANGUAGE RecordPuns #-}
module Main where

import Bustle.Types
import Bustle.Parser

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

import Control.Monad.State
import Control.Monad (forM_)

import Graphics.UI.Gtk hiding (get)
import Graphics.Rendering.Cairo
import Control.Monad.Trans ( liftIO )

import Debug.Trace (trace)

traceM x = trace (show x) (return ())

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
                , pending :: Map (BusName, Serial) (Message, (Double, Double))
                , row :: Double
                , mostRecentLabels :: Double
                }

modifyCoordinates f = modify (\bs -> bs { coordinates = f (coordinates bs)})

addPending m x = do
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

lastComponent :: BusName -> String
lastComponent = reverse . takeWhile (/= '.') . reverse

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

firstAppX = 150

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

memberName :: Message -> StateT BustleState Render ()
memberName m = do
    current <- gets row

    lift $ do
        moveTo 0 current
        showText $ path m

        moveTo 0 (current + 10)
        showText . abbreviate $ iface m ++ " . " ++ member m


returnArc m currentx = do
    ps <- gets pending
    currenty <- gets row

    case Map.lookup (destination m, inReplyTo m) ps of
        Nothing -> return ()
        Just (m, (callx, cally)) -> lift $ do
            let vdiff = (currenty - cally) / 3
                curvey1 = currenty - vdiff
                curvey2 = currenty - vdiff * 2

                direction = if callx > currentx then (+) else (-)

                curvex = (max currentx callx) `direction` 30

            setSourceRGB 0.4 0.7 0.4
            setDash [3, 3] 0

            moveTo currentx currenty
            lineTo curvex curvey1
            lineTo curvex curvey2
            lineTo callx cally
--            curveTo curvex curvey1 curvex curvey2 callx cally
            stroke

            setDash [] 0
            -- XXX Remove from pending

munge :: Message -> StateT BustleState Render ()
munge m = do
    advanceBy 30 -- FIXME: use some function of timestamp

    sc <- senderCoordinate m
    case m of
        Signal {}       -> memberName m >> signal sc

        MethodCall {}   -> do
            memberName m

            dc <- destinationCoordinate m
            methodCall sc dc

            addPending m dc

        MethodReturn {} -> do
            dc <- destinationCoordinate m

            methodReturn sc dc

            returnArc m sc

        Error {}        -> error "eh"

process' :: [Message] -> StateT BustleState Render ()
process' = mapM_ munge

process :: [Message] -> Render ()
process log = evalStateT (process' log) bs
    where bs = BustleState Map.empty Map.empty 0 0

halfArrowHead above left = do
    (x,y) <- getCurrentPoint
    let x' = if left then x - 10 else x + 10
    let y' = if above then y - 5 else y + 5
    if left -- work around weird artifacts
      then moveTo x' y' >> lineTo x y
      else lineTo x' y' >> moveTo x y

arrowHead left = halfArrowHead False left >> halfArrowHead True left

halfArrow above x x' = do
    t <- gets row
    lift $ do
        moveTo x t
        lineTo x' t
        halfArrowHead above (x < x')
        stroke

methodCall = halfArrow True
methodReturn = halfArrow False

signal x = do
    t <- gets row
    cs <- gets coordinates
    let (left, right) = (Map.fold min 10000 cs, Map.fold max 0 cs)
    lift $ do
        moveTo (left - 20) t
        arrowHead False
        lineTo (right + 20) t
        arrowHead True
        stroke

run :: Render () -> IO ()
run act = do
  initGUI
  window <- windowNew

  layout <- layoutNew Nothing Nothing
  layout `onExpose` updateLayout layout act
  layoutSetSize layout 5000 10000
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  containerAdd scrolledWindow layout
  containerAdd window scrolledWindow

  windowSetDefaultSize window 900 700
  widgetShowAll window

  window `onDestroy` mainQuit

  mainGUI

  where updateLayout :: Layout -> Render () -> Event -> IO Bool
        updateLayout layout act (Expose {}) = do
          win <- layoutGetDrawWindow layout
          renderWithDrawable win act
          return True
        updateLayout layout act _ = return False

-- vim: sw=2 sts=2
