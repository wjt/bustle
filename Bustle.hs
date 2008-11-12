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

import Prelude hiding (catch)
import Control.Exception (catch)

import Paths_bustle
import Bustle.Parser
import Bustle.Renderer
import Bustle.Types

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import System.Environment (getArgs)

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

  let (width, height, act) = process log

  window <- mkWindow filename

  layout <- layoutNew Nothing Nothing
  layoutSetSize layout (floor width) (floor height)
  layout `onExpose` updateLayout layout act
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWindow PolicyAutomatic PolicyAlways
  containerAdd scrolledWindow layout
  containerAdd window scrolledWindow
  windowSetDefaultSize window 900 700
  widgetShowAll window

  mainGUI

  where updateLayout :: Layout -> Render () -> Event -> IO Bool
        updateLayout layout act (Expose {}) = do
          win <- layoutGetDrawWindow layout

          hadj <- layoutGetHAdjustment layout
          hpos <- adjustmentGetValue hadj
          hpage <- adjustmentGetPageSize hadj

          vadj <- layoutGetVAdjustment layout
          vpos <- adjustmentGetValue vadj
          vpage <- adjustmentGetPageSize vadj

          print (hpos, vpos, hpage, vpage)

          renderWithDrawable win act
          return True
        updateLayout layout act _ = return False

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
