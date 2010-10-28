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
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             ScopedTypeVariables, FlexibleInstances #-}
module Main (main)
where

import Prelude hiding (log, catch)

import Control.Arrow ((&&&))
import Control.Exception
import Control.Monad (when, forM)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Data.IORef
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Version (showVersion)

import Paths_bustle
import Bustle.Parser
import Bustle.Renderer (process)
import Bustle.Types
import Bustle.Diagram
import Bustle.Upgrade (upgrade)

import System.Glib.GError (GError(..), catchGError)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import Graphics.Rendering.Cairo (withPDFSurface, renderWith)

import System.Process (runProcess)
import System.Environment (getArgs)
import System.FilePath (splitFileName, dropExtension)
import System.IO (hPutStrLn, stderr)

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

-}

newtype B a = B (ReaderT (IORef BState) IO a)
  deriving (Functor, Monad, MonadIO)

type Details = (FilePath, Diagram)
data WindowInfo = WindowInfo { wiWindow :: Window
                             , wiSave :: ImageMenuItem
                             , wiNotebook :: Notebook
                             , wiLayout :: Layout
                             }

data BState = BState { windows :: Int
                     , initialWindow :: Maybe WindowInfo
                     }

instance MonadState BState B where
  get = B $ ask >>= liftIO . readIORef
  put x = B $ ask >>= \r -> liftIO $ writeIORef r x

embedIO :: (IORef BState -> IO a) -> B a
embedIO act = B $ do
  r <- ask
  liftIO $ act r

makeCallback :: B a -> IORef BState -> IO a
makeCallback (B act) x = runReaderT act x

runB :: B a -> IO a
runB (B act) = runReaderT act =<< newIORef (BState 0 Nothing)

{- And now, some convenience functions -}

io :: MonadIO m => IO a -> m a
io = liftIO

modifyWindows :: (Int -> Int) -> B ()
modifyWindows f = modify $ \s -> s { windows = f (windows s) }

incWindows :: B ()
incWindows = modifyWindows (+1)

decWindows :: B Int
decWindows = modifyWindows (subtract 1) >> gets windows

{- End of boilerplate. -}

-- Used to log warnings which aren't important to the user, but which should
-- probably be noted.
warn :: String -> IO ()
warn = hPutStrLn stderr . ("Warning: " ++)

main :: IO ()
main = runB mainB

mainB :: B ()
mainB = do
  io initGUI

  -- Try to load arguments, if any.
  args <- io getArgs
  case args of
      ["--pair", sessionLogFile, systemLogFile] ->
          loadLog sessionLogFile (Just systemLogFile)
      _ -> mapM_ (\file -> loadLog file Nothing) args

  -- If no windows are open (because none of the arguments, if any, were loaded
  -- successfully) create an empty window
  n <- gets windows
  when (n == 0) createInitialWindow

  io mainGUI

createInitialWindow :: B ()
createInitialWindow = do
  misc <- emptyWindow
  modify $ \s -> s { initialWindow = Just misc }

loadInInitialWindow :: FilePath -> Maybe FilePath -> B ()
loadInInitialWindow = loadLogWith consumeInitialWindow
  where consumeInitialWindow = do
          x <- gets initialWindow
          case x of
            Nothing   -> emptyWindow
            Just misc -> do
              modify $ \s -> s { initialWindow = Nothing }
              return misc

loadLog :: FilePath -> Maybe FilePath -> B ()
loadLog = loadLogWith emptyWindow

-- Displays a modal error dialog, with the given strings as title and body
-- respectively.
displayError :: String -> String -> IO ()
displayError title body = do
  dialog <- messageDialogNew Nothing [DialogModal] MessageError ButtonsClose title
  messageDialogSetSecondaryText dialog body
  dialog `afterResponse` \_ -> widgetDestroy dialog
  widgetShowAll dialog

-- Converts an Either to an action in an ErrorT.
toET :: (Monad m, Error e') => (e -> e') -> Either e a -> ErrorT e' m a
toET f = either (throwError . f) return

-- Catches IOExceptions , and maps them into ErrorT
etio :: (Error e', MonadIO io)
     => (IOException -> e') -> IO a -> ErrorT e' io a
etio f act = toET f =<< io (try act)

-- This needs FlexibleInstances and I don't know why
instance Error (String, String) where
    strMsg s = ("", s)
    noMsg = ("", "")

loadLogWith :: B WindowInfo   -- ^ action returning a window to load the log(s) in
            -> FilePath       -- ^ a log file to load and display
            -> Maybe FilePath -- ^ an optional second log to show alongside the
                              --   first log.
            -> B ()
loadLogWith getWindow session maybeSystem = do
    ret <- runErrorT $ do
        sessionMessages <- readLogFile session
        systemMessages <- case maybeSystem of
            Just system -> readLogFile system
            Nothing     -> return []

        -- FIXME: pass the log file name into the renderer
        shapes <- toET (\e -> ("one of the logs", e)) $
                      process (upgrade sessionMessages)
                              (upgrade systemMessages)

        windowInfo <- lift getWindow
        let title = case maybeSystem of
                Just system -> session ++ " and " ++ system
                Nothing     -> session

        lift $ displayLog windowInfo title shapes

    case ret of
      Left (f, e) -> io $ displayError ("Could not read '" ++ f ++ "'") e
      Right () -> return ()

  where readLogFile f = do
            input <- etio (\e -> (f, show e)) $ readFile f
            toET (\e -> (f, "Parse error " ++ show e)) $ readLog input


maybeQuit :: B ()
maybeQuit = do
  n <- decWindows
  when (n == 0) (io mainQuit)

emptyWindow :: B WindowInfo
emptyWindow = do
  Just xml <- io $ xmlNew =<< getDataFileName "bustle.glade"

  -- Grab a bunch of widgets. Surely there must be a better way to do this?
  let getW cast name = io $ xmlGetWidget xml cast name

  window <- getW castToWindow "diagramWindow"
  [openItem, saveItem, closeItem, aboutItem] <- mapM (getW castToImageMenuItem)
       ["open", "saveAs", "close", "about"]
  openTwoItem <- getW castToMenuItem "openTwo"
  layout <- getW castToLayout "diagramLayout"
  nb <- getW castToNotebook "notebook"

  -- Open two logs dialog widgets
  openTwoDialog <- getW castToDialog "openTwoDialog"
  [sessionBusChooser, systemBusChooser] <- mapM (getW castToFileChooserButton)
      ["sessionBusChooser", "systemBusChooser"]

  let windowInfo = WindowInfo { wiWindow = window
                              , wiSave = saveItem
                              , wiNotebook = nb
                              , wiLayout = layout
                              }

  -- Set up the window itself
  io $ withIcon (windowSetIcon window)
  embedIO $ onDestroy window . makeCallback maybeQuit

  -- File menu
  embedIO $ onActivateLeaf openItem . makeCallback (openDialogue window)
  io $ openTwoItem `onActivateLeaf` widgetShowAll openTwoDialog
  io $ closeItem `onActivateLeaf` widgetDestroy window

  -- Help menu
  io $ onActivateLeaf aboutItem (showAbout window)

  -- Diagram area panning
  io $ do
    hadj <- layoutGetHAdjustment layout
    vadj <- layoutGetVAdjustment layout

    window `on` keyPressEvent $ tryEvent $ do
      key <- eventKeyName
      case key of
        "Up"        -> io $ decStep vadj
        "Down"      -> io $ incStep vadj
        "Left"      -> io $ decStep hadj
        "Right"     -> io $ incStep hadj
        "Page_Down" -> io $ incPage vadj
        "space"     -> io $ incPage vadj
        "Page_Up"   -> io $ decPage vadj
        _           -> stopEvent

    widgetShowAll window

  -- Open two logs dialog
  io $ do
    withIcon (windowSetIcon openTwoDialog)
    windowSetTransientFor openTwoDialog window
    openTwoDialog `on` deleteEvent $ tryEvent $ io $ widgetHide openTwoDialog

    -- Keep the two dialogs' current folders in sync. We only propagate when
    -- the new dialog doesn't have a current file. Otherwise, choosing a file
    -- from a different directory in the second chooser unselects the first.
    let propagateCurrentFolder d1 d2 = do
            d1 `onCurrentFolderChanged` do
                f1 <- fileChooserGetCurrentFolder d1
                f2 <- fileChooserGetCurrentFolder d2
                otherFile <- fileChooserGetFilename d2
                when (isNothing otherFile && f1 /= f2 && isJust f1) $ do
                    fileChooserSetCurrentFolder d2 (fromJust f1)
                    return ()

    propagateCurrentFolder sessionBusChooser systemBusChooser
    propagateCurrentFolder systemBusChooser sessionBusChooser

  let hideTwoDialog = do
          widgetHideAll openTwoDialog
          fileChooserUnselectAll sessionBusChooser
          fileChooserUnselectAll systemBusChooser

  embedIO $ \r -> openTwoDialog `afterResponse` \resp -> do
      -- The "Open" button should only be sensitive if both pickers have a
      -- file in them, but the GtkFileChooserButton:file-set signal is not
      -- bound in my version of Gtk2Hs. So yeah...
      if (resp == ResponseAccept)
        then do
          sessionLogFile <- fileChooserGetFilename sessionBusChooser
          systemLogFile <- fileChooserGetFilename systemBusChooser

          case (sessionLogFile, systemLogFile) of
            (Just f1, Just f2) -> do
                makeCallback (loadInInitialWindow f1 (Just f2)) r
                hideTwoDialog
            _ -> return ()
        else
          hideTwoDialog

  incWindows
  return windowInfo

displayLog :: WindowInfo -> FilePath -> Diagram -> B ()
displayLog (WindowInfo { wiWindow = window
                       , wiSave = saveItem
                       , wiLayout = layout
                       , wiNotebook = nb
                       })
           filename shapes = do
  let (width, height) = diagramDimensions shapes
      details = (filename, shapes)

  io $ do
    windowSetTitle window $
        snd (splitFileName filename) ++ " — D-Bus Sequence Diagram"

    widgetSetSensitivity saveItem True
    onActivateLeaf saveItem $ saveToPDFDialogue window details

    layoutSetSize layout (floor width) (floor height)
    layout `on` exposeEvent $ liftIO (update layout shapes) >> return True

    notebookSetCurrentPage nb 1

    return ()

update :: Layout -> Diagram -> IO ()
update layout shapes = do
  win <- layoutGetDrawWindow layout

  hadj <- layoutGetHAdjustment layout
  hpos <- adjustmentGetValue hadj
  hpage <- adjustmentGetPageSize hadj

  vadj <- layoutGetVAdjustment layout
  vpos <- adjustmentGetValue vadj
  vpage <- adjustmentGetPageSize vadj

  let r = (hpos, vpos, hpos + hpage, vpos + vpage)

  renderWithDrawable win $ drawRegion r False shapes

-- Add/remove one step/page increment from an Adjustment, limited to the top of
-- the last page.
incStep, decStep, incPage, decPage :: Adjustment -> IO ()
incStep = incdec (+) adjustmentGetStepIncrement
decStep = incdec (-) adjustmentGetStepIncrement
incPage = incdec (+) adjustmentGetPageIncrement
decPage = incdec (-) adjustmentGetPageIncrement

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

withIcon :: (Maybe Pixbuf -> IO ()) -> IO ()
withIcon act = do
  iconName <- getDataFileName "bustle.png"
  pb <- (fmap Just (pixbufNewFromFile iconName)) `catchGError`
    \(GError _ _ msg) -> warn msg >> return Nothing
  act pb

openDialogue :: Window -> B ()
openDialogue window = embedIO $ \r -> do
  chooser <- fileChooserDialogNew Nothing (Just window) FileChooserActionOpen
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-open", ResponseAccept)
             ]
  chooser `set` [ windowModal := True
                , fileChooserLocalOnly := True
                ]

  chooser `afterResponse` \resp -> do
      when (resp == ResponseAccept) $ do
          Just fn <- fileChooserGetFilename chooser
          makeCallback (loadInInitialWindow fn Nothing) r
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

  chooser `afterResponse` \resp -> do
      when (resp == ResponseAccept) $ do
          Just fn <- io $ fileChooserGetFilename chooser
          let (width, height) = diagramDimensions shapes
          withPDFSurface fn width height $
            \surface -> renderWith surface $ drawDiagram False shapes
      widgetDestroy chooser

  widgetShowAll chooser

showAbout :: Window -> IO ()
showAbout window = do
    dialog <- aboutDialogNew

    license <- (Just `fmap` (readFile =<< getDataFileName "LICENSE"))
               `catch` (\e -> warn (show (e :: IOException)) >> return Nothing)

    dialog `set` [ aboutDialogName := "Bustle"
                 , aboutDialogVersion := showVersion version
                 , aboutDialogComments := "Someone's favourite D-Bus profiler"
                 , aboutDialogWebsite := "http://willthompson.co.uk/bustle"
                 , aboutDialogAuthors := authors
                 , aboutDialogCopyright := "© 2008–2009 Collabora Ltd."
                 , aboutDialogLicense := license
                 ]
    dialog `afterResponse` \resp ->
        when (resp == ResponseCancel) (widgetDestroy dialog)
    windowSetTransientFor dialog window
    windowSetModal dialog True
    withIcon (aboutDialogSetLogo dialog)

    widgetShowAll dialog

authors :: [String]
authors = [ "Will Thompson <will.thompson@collabora.co.uk>"
          , "Dafydd Harries"
          , "Chris Lamb"
          , "Marc Kleine-Budde"
          ]

-- vim: sw=2 sts=2
