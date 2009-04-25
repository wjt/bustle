{-
Bustle: a tool to draw charts of D-Bus activity
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
module Main where

import Prelude hiding (log)

import Control.Arrow ((&&&))
import Control.Monad (when, forM)

import Data.IORef

import Paths_bustle
import Bustle.Parser
import Bustle.Renderer
import Bustle.Types
import Bustle.Diagram
import Bustle.Upgrade (upgrade)

import System.Glib.GError (catchGError)
import Graphics.UI.Gtk
-- FIXME: Events is deprecated in favour of EventM
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo

import System.Environment (getArgs)
import System.FilePath (splitFileName, dropExtension)

main :: IO ()
main = do
    args <- getArgs
    case args of
      []  -> do putStrLn "Usage: bustle log-file [another-log-file ...]"
                putStrLn "See the README"
      fs  -> run fs

run :: [FilePath] -> IO ()
run fs = do
  initGUI

  nwindows <- newIORef 0

  hoorays <- mapM (loadLog nwindows) fs

  when (or hoorays) mainGUI

loadLog :: IORef Int -> FilePath -> IO Bool
loadLog nwindows f = do
  input <- readFile f
  case readLog input of
    Left err -> do putStrLn $ concat [ "Couldn't parse "
                                     , f
                                     , ": "
                                     , show err
                                     ]
                   return False
    Right log -> aWindow f (upgrade log) nwindows >> return True

maybeQuit :: IORef Int -> IO ()
maybeQuit nwindows = do
    modifyIORef nwindows (subtract 1)
    n <- readIORef nwindows
    when (n == 0) mainQuit

aWindow :: FilePath -> [Message] -> IORef Int -> IO ()
aWindow filename log nwindows = do
  let shapes = process log
      (width, height) = dimensions shapes

  window <- mkWindow filename nwindows
  vbox <- vBoxNew False 0
  containerAdd window vbox

  menuBar <- mkMenuBar window filename shapes width height
  boxPackStart vbox menuBar PackNatural 0

  layout <- layoutNew Nothing Nothing
  layoutSetSize layout (floor width) (floor height)
  layout `onExpose` update layout shapes
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWindow PolicyAutomatic PolicyAlways
  containerAdd scrolledWindow layout
  boxPackStart vbox scrolledWindow PackGrow 0
  windowSetDefaultSize window 900 700

  hadj <- layoutGetHAdjustment layout
  adjustmentSetStepIncrement hadj 50
  vadj <- layoutGetVAdjustment layout
  adjustmentSetStepIncrement vadj 50

  window `onKeyPress` \event -> case event of
      Key { eventKeyName=kn } -> case kn of
        "Up"    -> dec vadj
        "Down"  -> inc vadj
        "Left"  -> dec hadj
        "Right" -> inc hadj
        _ -> return False
      _ -> return False

  widgetShowAll window
  modifyIORef nwindows (+1)

  where update :: Layout -> Diagram -> Event -> IO Bool
        update layout shapes (Expose {}) = do
          win <- layoutGetDrawWindow layout

          hadj <- layoutGetHAdjustment layout
          hpos <- adjustmentGetValue hadj
          hpage <- adjustmentGetPageSize hadj

          vadj <- layoutGetVAdjustment layout
          vpos <- adjustmentGetValue vadj
          vpage <- adjustmentGetPageSize vadj

          let r = (hpos, vpos, hpos + hpage, vpos + vpage)

          renderWithDrawable win $ drawRegion r False shapes
          return True
        update _layout _act _ = return False

-- Add or remove one step increment from an Adjustment, limited to the top of
-- the last page.
inc, dec :: Adjustment -> IO Bool
inc = incdec (+)
dec = incdec (-)

incdec :: (Double -> Double -> Double) -> Adjustment -> IO Bool
incdec (+-) adj = do
    pos <- adjustmentGetValue adj
    step <- adjustmentGetStepIncrement adj
    page <- adjustmentGetPageSize adj
    lim <- adjustmentGetUpper adj
    adjustmentSetValue adj $ min (pos +- step) (lim - page)
    return True

mkWindow :: FilePath -> IORef Int -> IO Window
mkWindow filename nwindows = do
    window <- windowNew
    windowSetTitle window $ filename ++ " - D-Bus Sequence Diagram"
    window `onDestroy` maybeQuit nwindows

    iconName <- getDataFileName "bustle.png"
    let load x = pixbufNewFromFile x >>= windowSetIcon window
    foldl1 (\m n -> m `catchGError` const n)
      [ load iconName
      , load "bustle.png"
      , putStrLn "Couldn't find window icon. Oh well."
      ]

    return window

saveToPDFDialogue :: Window -> FilePath -> Diagram -> Double -> Double -> IO ()
saveToPDFDialogue window filename shapes width height = do
  chooser <- fileChooserDialogNew Nothing (Just window) FileChooserActionSave
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-save", ResponseAccept)
             ]
  chooser `set` [ windowModal := True
                , fileChooserLocalOnly := True
                , fileChooserDoOverwriteConfirmation := True
                ]

  let (dir, base) = splitFileName filename
  fileChooserSetCurrentFolder chooser dir
  fileChooserSetCurrentName chooser $ dropExtension base ++ ".pdf"

  chooser `afterResponse` \response -> do
      when (response == ResponseAccept) $ do
          Just fn <- fileChooserGetFilename chooser
          withPDFSurface fn width height $
            \surface -> renderWith surface $ drawDiagram False shapes
      widgetDestroy chooser

  widgetShowAll chooser


mkMenuBar :: Window -> FilePath -> Diagram -> Double -> Double
          -> IO MenuBar
mkMenuBar window filename shapes width height = do
  menuBar <- menuBarNew

  file <- menuItemNewWithMnemonic "_File"
  fileMenu <- menuNew
  menuItemSetSubmenu file fileMenu

  saveItem <- imageMenuItemNewFromStock stockSaveAs
  menuShellAppend fileMenu saveItem
  onActivateLeaf saveItem $ saveToPDFDialogue window filename shapes width height

  menuShellAppend fileMenu =<< separatorMenuItemNew

  closeItem <- imageMenuItemNewFromStock stockClose
  menuShellAppend fileMenu closeItem
  closeItem `onActivateLeaf` widgetDestroy window

  menuShellAppend menuBar file

  return menuBar

-- vim: sw=2 sts=2
