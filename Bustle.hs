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
module Main where

import Prelude hiding (catch, log)
import Control.Exception (catch)

import Control.Arrow ((&&&))

import Paths_bustle
import Bustle.Parser
import Bustle.Renderer
import Bustle.Types
import Bustle.Diagram

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [f] -> do input <- readFile f
                case readLog input of
                  Left err -> putStrLn $ concat [ "Couldn't parse "
                                                , f
                                                , ": "
                                                , show err
                                                ]
                  Right log -> run f log
      _   -> do putStrLn "Usage: bustle log-file-name"
                putStrLn "See the README"

run :: FilePath -> [Message] -> IO ()
run filename log = do
  initGUI

  let shapes :: [(Rect, Shape)]
      shapes = map (bounds &&& id) $ process log

      (width, height) = (maximum *** maximum) $
                          unzip [ (x2, y2) | ((_, _, x2, y2), _) <- shapes ]

  window <- mkWindow filename

  layout <- layoutNew Nothing Nothing
  layoutSetSize layout (floor width) (floor height)
  layout `onExpose` update layout shapes
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWindow PolicyAutomatic PolicyAlways
  containerAdd scrolledWindow layout
  containerAdd window scrolledWindow
  windowSetDefaultSize window 900 700
  widgetShowAll window

  mainGUI

  where update :: Layout -> [(Rect, Shape)] -> Event -> IO Bool
        update layout shapes (Expose {}) = do
          win <- layoutGetDrawWindow layout

          hadj <- layoutGetHAdjustment layout
          hpos <- adjustmentGetValue hadj
          hpage <- adjustmentGetPageSize hadj

          vadj <- layoutGetVAdjustment layout
          vpos <- adjustmentGetValue vadj
          vpage <- adjustmentGetPageSize vadj

          let r = (hpos, vpos, hpos + hpage, vpos + vpage)

          renderWithDrawable win $ clearCanvas >> drawVisible r shapes
          return True
        update _layout _act _ = return False

clearCanvas :: Render ()
clearCanvas = do
    save
    setSourceRGB 1 1 1
    setOperator OperatorSource
    paint
    restore

visibleShapes :: Rect -> [(Rect, Shape)] -> [Shape]
visibleShapes r = map snd . filter (intersects r . fst)

drawVisible :: Rect -> [(Rect, Shape)] -> Render ()
drawVisible r = mapM_ draw . visibleShapes r

mkWindow :: FilePath -> IO Window
mkWindow filename = do
    window <- windowNew
    windowSetTitle window $ filename ++ " - D-Bus Activity Visualizer"
    window `onDestroy` mainQuit

    iconName <- getDataFileName "bustle.png"
    let load x = pixbufNewFromFile x >>= windowSetIcon window
    foldl1 (\m n -> m `catch` const n)
      [ load iconName
      , load "bustle.png"
      , putStrLn "Couldn't find window icon. Oh well."
      ]

    return window

-- vim: sw=2 sts=2
