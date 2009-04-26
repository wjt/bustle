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
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main where

import Prelude hiding (log)

import Control.Arrow ((&&&))
import Control.Monad (when, forM)
import Control.Monad.Reader
import Control.Monad.State

import Data.IORef
import qualified Data.Set as Set
import Data.Set (Set)

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

{-
Cunning threadable monad. Inspired by Monadic Tunnelling
<http://www.haskell.org/pipermail/haskell-cafe/2007-July/028501.html>

This is a state monad, secretly implemented with an IORef. The idea is to make
it possible to reconstitute the monad within a Gtk callback. Given:

  x :: Badger
  onDance :: Badger -> IO a -> IO ()
  dancedCB :: B a

  onMeme :: Badger -> (Mushroom -> IO a) -> IO ()
  memedCB :: Mushroom -> B a

One can write:

  embedIO $ onDance x . makeCallback dancedCB
  embedIO $ \r -> onMeme x (reconstruct r . dancedCB)

I'm not sure which of makeCallback and reconstruct are more useful.
-}

newtype B a = B (ReaderT (IORef BState) IO a)
  deriving (Functor, Monad, MonadIO)

type Details = (FilePath, Diagram)

data BState = BState { windows :: Int }

instance MonadState BState B where
  get = B $ ask >>= liftIO . readIORef
  put x = B $ ask >>= \r -> liftIO $ writeIORef r x

embedIO :: (IORef BState -> IO a) -> B a
embedIO act = B $ do
  r <- ask
  liftIO $ act r

makeCallback :: B a -> IORef BState -> IO a
makeCallback (B act) x = runReaderT act x

reconstruct :: IORef BState -> B a -> IO a
reconstruct = flip makeCallback

runB :: B a -> IO a
runB (B act) = runReaderT act =<< newIORef (BState 0)

{- And now, some convenience functions -}

io :: MonadIO m => IO a -> m a
io = liftIO

modifyWindows :: (Int -> Int) -> B ()
modifyWindows f = modify $ \(BState n) -> BState (f n)

incWindows :: B ()
incWindows = modifyWindows (+1)

decWindows :: B Int
decWindows = modifyWindows (subtract 1) >> gets windows

{- End of boilerplate. -}

main :: IO ()
main = runB mainB

mainB :: B ()
mainB = do
  io initGUI

  fs <- io getArgs

  if null fs
    then emptyWindow >> return ()
    else mapM_ loadLog fs

  n <- gets windows
  when (n > 0) (io mainGUI)

loadLog :: FilePath -> B ()
loadLog f = do
  input <- io $ readFile f
  case readLog input of
    Left err -> io . putStrLn $ concat [ "Couldn't parse "
                                       , f
                                       , ": "
                                       , show err
                                       ]
    Right log -> logWindow f (upgrade log)

maybeQuit :: B ()
maybeQuit = do
  n <- decWindows
  when (n == 0) (io mainQuit)

emptyWindow :: B (Window, ImageMenuItem, Layout)
emptyWindow = do
  window <- mkWindow
  (menuBar, saveItem) <- mkMenuBar window
  layout <- io $ layoutNew Nothing Nothing

  io $ do
    vbox <- vBoxNew False 0
    containerAdd window vbox

    boxPackStart vbox menuBar PackNatural 0

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

  incWindows
  return (window, saveItem, layout)


logWindow :: FilePath -> [Message] -> B ()
logWindow filename log = do
  (window, saveItem, layout) <- emptyWindow

  let shapes = process log
      (width, height) = dimensions shapes
      details = (filename, shapes)

  io $ do
    windowSetTitle window $ filename ++ " - D-Bus Sequence Diagram"

    widgetSetSensitive saveItem True
    onActivateLeaf saveItem $ saveToPDFDialogue window details

    layout `onExpose` update layout shapes
    layoutSetSize layout (floor width) (floor height)

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

mkWindow :: B Window
mkWindow = do
    window <- io windowNew

    io $ do
      windowSetTitle window "D-Bus Sequence Diagram"
      iconName <- getDataFileName "bustle.png"
      let load x = pixbufNewFromFile x >>= windowSetIcon window
      foldl1 (\m n -> m `catchGError` const n)
        [ load iconName
        , load "bustle.png"
        , putStrLn "Couldn't find window icon. Oh well."
        ]

    embedIO $ onDestroy window . makeCallback maybeQuit

    return window

openDialogue :: Window -> B ()
openDialogue window = embedIO $ \r -> do
  chooser <- fileChooserDialogNew Nothing (Just window) FileChooserActionOpen
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-open", ResponseAccept)
             ]
  chooser `set` [ windowModal := True
                , fileChooserLocalOnly := True
                ]

  chooser `afterResponse` \response -> do
      when (response == ResponseAccept) $ do
          Just fn <- fileChooserGetFilename chooser
          makeCallback (loadLog fn) r
      widgetDestroy chooser

  widgetShowAll chooser

saveToPDFDialogue :: Window -> Details -> IO ()
saveToPDFDialogue window (filename, shapes) = do
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
          Just fn <- io $ fileChooserGetFilename chooser
          let (width, height) = dimensions shapes
          withPDFSurface fn width height $
            \surface -> renderWith surface $ drawDiagram False shapes
      widgetDestroy chooser

  widgetShowAll chooser


mkMenuBar :: Window -> B (MenuBar, ImageMenuItem)
mkMenuBar window = embedIO $ \r -> do
  menuBar <- menuBarNew

  file <- menuItemNewWithMnemonic "_File"
  fileMenu <- menuNew
  menuItemSetSubmenu file fileMenu

  openItem <- imageMenuItemNewFromStock stockOpen
  menuShellAppend fileMenu openItem
  onActivateLeaf openItem $ reconstruct r (openDialogue window)

  saveItem <- imageMenuItemNewFromStock stockSaveAs
  menuShellAppend fileMenu saveItem
  widgetSetSensitive saveItem False

  menuShellAppend fileMenu =<< separatorMenuItemNew

  closeItem <- imageMenuItemNewFromStock stockClose
  menuShellAppend fileMenu closeItem
  closeItem `onActivateLeaf` widgetDestroy window

  menuShellAppend menuBar file

  return (menuBar, saveItem)

-- vim: sw=2 sts=2
