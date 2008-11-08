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

import Bustle.Parser
import Bustle.Renderer

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

main = do
    input <- readFile "/tmp/face"
    let Right log = readLog input
    run $ process log

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
