{-
Bustle.UI: displays charts of D-Bus activity
Copyright © 2008–2011 Collabora Ltd.

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
{-# LANGUAGE ScopedTypeVariables #-}
module Bustle.UI
  ( uiMain
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Data.IORef
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Time
import Data.Monoid (mempty)
import Text.Printf

import Paths_bustle
import Bustle.Application.Monad
import Bustle.Renderer
import Bustle.Types
import Bustle.Diagram
import Bustle.Marquee (toString)
import Bustle.Util
import Bustle.UI.AboutDialog
import Bustle.UI.Canvas
import Bustle.UI.DetailsView
import Bustle.UI.FilterDialog
import Bustle.UI.OpenTwoDialog (setupOpenTwoDialog)
import Bustle.UI.Recorder
import Bustle.UI.Util (displayError)
import Bustle.StatisticsPane
import Bustle.Translation (__)
import Bustle.Loader

import qualified Control.Exception as C
import System.Glib.GError (GError(..), failOnGError)

import Graphics.UI.Gtk

import Graphics.Rendering.Cairo (withPDFSurface, renderWith)

import System.FilePath ( splitFileName, takeFileName, takeDirectory
                       , dropExtension, dropTrailingPathSeparator
                       , (</>), (<.>)
                       )
import System.Directory (renameFile)

type B a = Bustle BConfig BState a

data LogDetails =
    RecordedLog FilePath
  | SingleLog FilePath
  | TwoLogs FilePath FilePath

data Page =
    InstructionsPage
  | PleaseHoldPage
  | CanvasPage
  deriving
    (Enum)

data WindowInfo =
    WindowInfo { wiWindow :: Window
               , wiSave :: ImageMenuItem
               , wiExport :: MenuItem
               , wiViewStatistics :: CheckMenuItem
               , wiFilterNames :: MenuItem
               , wiNotebook :: Notebook
               , wiStatsBook :: Notebook
               , wiStatsPane :: StatsPane
               , wiContentVPaned :: VPaned
               , wiCanvas :: Canvas (Detailed Message)
               , wiDetailsView :: DetailsView

               , wiLogDetails :: IORef (Maybe LogDetails)
               }

data BConfig =
    BConfig { debugEnabled :: Bool
            , methodIcon :: Maybe Pixbuf
            , signalIcon :: Maybe Pixbuf
            }

data BState = BState { windows :: Int
                     , initialWindow :: Maybe WindowInfo
                     }

modifyWindows :: (Int -> Int) -> B ()
modifyWindows f = modify $ \s -> s { windows = f (windows s) }

incWindows :: B ()
incWindows = modifyWindows (+1)

decWindows :: B Int
decWindows = modifyWindows (subtract 1) >> gets windows

uiMain :: IO ()
uiMain = failOnGError $ do
    args <- initGUI

    -- FIXME: get a real option parser
    let debug = any isDebug args

    [method, signal] <- mapM loadPixbuf
        ["dfeet-method.png", "dfeet-signal.png"]

    let config = BConfig { debugEnabled = debug
                         , methodIcon = method
                         , signalIcon = signal
                         }
        initialState = BState { windows = 0
                              , initialWindow = Nothing
                              }

    runB config initialState $ mainB (filter (not . isDebug) args)
  where
    isDebug = (== "--debug")

mainB :: [String] -> B ()
mainB args = do
  case args of
      ["--pair", sessionLogFile, systemLogFile] ->
          loadLog (TwoLogs sessionLogFile systemLogFile)
      _ -> mapM_ (loadLog . SingleLog) args

  -- If no windows are open (because none of the arguments, if any, were loaded
  -- successfully) create an empty window
  n <- gets windows
  when (n == 0) createInitialWindow

  io mainGUI

createInitialWindow :: B ()
createInitialWindow = do
  misc <- emptyWindow
  modify $ \s -> s { initialWindow = Just misc }

consumeInitialWindow :: B WindowInfo
consumeInitialWindow = do
    x <- gets initialWindow
    case x of
        Nothing   -> emptyWindow
        Just windowInfo -> do
            modify $ \s -> s { initialWindow = Nothing }
            return windowInfo

loadInInitialWindow :: LogDetails -> B ()
loadInInitialWindow = loadLogWith consumeInitialWindow

loadLog :: LogDetails -> B ()
loadLog = loadLogWith emptyWindow

openLog :: MonadIO io
        => LogDetails
        -> ErrorT LoadError io ( ([String], [DetailedEvent])
                               , ([String], [DetailedEvent])
                               )
openLog (RecordedLog filepath) = do
    result <- readLog filepath
    return (result, ([], []))
openLog (SingleLog filepath) = do
    result <- readLog filepath
    return (result, ([], []))
openLog (TwoLogs session system) = do
    sessionResult <- readLog session
    systemResult <- readLog system
    return (sessionResult, systemResult)

loadLogWith :: B WindowInfo   -- ^ action returning a window to load the log(s) in
            -> LogDetails
            -> B ()
loadLogWith getWindow logDetails = do
    ret <- runErrorT $ do
        ((sessionWarnings, sessionMessages),
         (systemWarnings, systemMessages)) <- openLog logDetails

        -- FIXME: pass the log file name into the renderer
        let rr = process sessionMessages systemMessages
        io $ mapM warn $ sessionWarnings ++ systemWarnings ++ rrWarnings rr

        windowInfo <- lift getWindow
        lift $ displayLog windowInfo
                          logDetails
                          sessionMessages
                          systemMessages
                          rr

    case ret of
      Left (LoadError f e) -> io $
          displayError Nothing (printf (__ "Could not read '%s'") f) (Just e)
      Right () -> return ()

startRecording :: B ()
startRecording = do
    wi <- consumeInitialWindow

    zt <- io $ getZonedTime
    -- I hate time manipulation
    let yyyy_mm_dd_hh_mm_ss = takeWhile (/= '.') (show zt)

    cacheDir <- io $ getCacheDir
    let filename = cacheDir </> yyyy_mm_dd_hh_mm_ss <.> "bustle"

    setPage wi PleaseHoldPage
    let mwindow = Just (wiWindow wi)
        progress = aChallengerAppears wi
        finished = finishedRecording wi filename
    embedIO $ \r -> recorderRun filename mwindow progress
                                (\p -> makeCallback (finished p) r)

aChallengerAppears :: WindowInfo
                   -> RendererResult a
                   -> IO ()
aChallengerAppears wi rr = do
    updateDisplayedLog wi rr
    canvasScrollToBottom (wiCanvas wi)
    setPage wi CanvasPage

finishedRecording :: WindowInfo
                  -> FilePath
                  -> Bool
                  -> B ()
finishedRecording wi tempFilePath producedOutput = do
    if producedOutput
      then do
        -- TODO: There is a noticable lag when reloading big files. It would be
        -- nice to either make the loading faster, or eliminate the reload.
        loadLogWith (return wi) (RecordedLog tempFilePath)

        let saveItem     = wiSave wi

        io $ do
            widgetSetSensitivity saveItem True
            onActivateLeaf saveItem $ showSaveDialog wi (return ())
        return ()
      else do
        setPage wi InstructionsPage
        modify $ \s -> s { initialWindow = Just wi }
        updateDisplayedLog wi (mempty :: RendererResult ())

showSaveDialog :: WindowInfo
               -> IO ()
               -> IO ()
showSaveDialog wi savedCb = do
    Just (RecordedLog tempFilePath) <- readIORef (wiLogDetails wi)
    let mwindow      = Just (wiWindow wi)
        tempFileName = takeFileName tempFilePath

    recorderChooseFile tempFileName mwindow $ \newFilePath -> do
        renameFile tempFilePath newFilePath
        widgetSetSensitivity (wiSave wi) False
        wiSetLogDetails wi (SingleLog newFilePath)
        savedCb

-- | Show a confirmation dialog if the log is unsaved. Suitable for use as a
--   'delete-event' handler.
promptToSave :: MonadIO io
             => WindowInfo
             -> io Bool -- ^ True if we showed a prompt; False if we're
                        --   happy to quit
promptToSave wi = io $ do
    mdetails <- readIORef (wiLogDetails wi)
    case mdetails of
        Just (RecordedLog tempFilePath) -> do
            let tempFileName = takeFileName tempFilePath
                title = printf (__ "Save log '%s' before closing?") tempFileName :: String
            prompt <- messageDialogNew (Just (wiWindow wi))
                                       [DialogModal]
                                       MessageWarning
                                       ButtonsNone
                                       title
            messageDialogSetSecondaryText prompt
                (__ "If you don't save, this log will be lost forever.")
            dialogAddButton prompt (__ "Close _Without Saving") ResponseClose
            dialogAddButton prompt stockCancel ResponseCancel
            dialogAddButton prompt stockSave ResponseYes

            widgetShowAll prompt
            prompt `afterResponse` \resp -> do
                let closeUp = widgetDestroy (wiWindow wi)
                case resp of
                    ResponseYes -> showSaveDialog wi closeUp
                    ResponseClose -> closeUp
                    _ -> return ()
                widgetDestroy prompt

            return True
        _ -> return False

maybeQuit :: B ()
maybeQuit = do
  n <- decWindows
  when (n == 0) (io mainQuit)

emptyWindow :: B WindowInfo
emptyWindow = do
  builder <- io builderNew
  io $ builderAddFromFile builder =<< getDataFileName "data/bustle.ui"

  -- Grab a bunch of widgets. Surely there must be a better way to do this?
  let getW cast name = io $ builderGetObject builder cast name

  window <- getW castToWindow "diagramWindow"
  [newItem, openItem, saveItem, closeItem, aboutItem] <-
      mapM (getW castToImageMenuItem)
          ["new", "open", "save", "close", "about"]
  [newButton, openButton] <- mapM (getW castToButton) ["newButton", "openButton"]
  exportItem <- getW castToMenuItem "export"
  openTwoItem <- getW castToMenuItem "openTwo"
  viewStatistics <- getW castToCheckMenuItem "statistics"
  filterNames <- getW castToMenuItem "filter"


  [nb, statsBook] <- mapM (getW castToNotebook)
      ["diagramOrNot", "statsBook"]
  contentVPaned <- getW castToVPaned "contentVPaned"

  -- Open two logs dialog
  openTwoDialog <- embedIO $ \r ->
      setupOpenTwoDialog builder window $ \f1 f2 ->
          makeCallback (loadInInitialWindow (TwoLogs f1 f2)) r

  -- Set up the window itself
  embedIO $ onDestroy window . makeCallback maybeQuit

  -- File menu and related buttons
  embedIO $ \r -> do
      let new = makeCallback startRecording r
      onActivateLeaf newItem new
      onClicked newButton new

      let open = makeCallback (openDialogue window) r
      onActivateLeaf openItem open
      onClicked openButton open

      onActivateLeaf openTwoItem $ widgetShowAll openTwoDialog

  -- Help menu
  io $ onActivateLeaf aboutItem $ showAboutDialog window

  m <- asks methodIcon
  s <- asks signalIcon
  statsPane <- io $ statsPaneNew builder m s

  details <- io $ detailsViewNew
  io $ do
      let top = detailsViewGetTop details
      panedPack2 contentVPaned top False False
      -- Hide the details by default; they'll be shown when the user selects a
      -- message.
      widgetHide top

  -- The stats start off hidden.
  io $ widgetHide statsBook

  showBounds <- asks debugEnabled
  canvas <- io $ canvasNew builder showBounds (updateDetailsView details)

  logDetailsRef <- io $ newIORef Nothing
  let windowInfo = WindowInfo { wiWindow = window
                              , wiSave = saveItem
                              , wiExport = exportItem
                              , wiViewStatistics = viewStatistics
                              , wiFilterNames = filterNames
                              , wiNotebook = nb
                              , wiStatsBook = statsBook
                              , wiStatsPane = statsPane
                              , wiContentVPaned = contentVPaned
                              , wiCanvas = canvas
                              , wiDetailsView = details
                              , wiLogDetails = logDetailsRef
                              }

  io $ window `on` deleteEvent $ promptToSave windowInfo
  io $ closeItem `on` menuItemActivate $ do
      prompted <- promptToSave windowInfo
      when (not prompted) (widgetDestroy window)
  incWindows
  io $ widgetShow window
  return windowInfo

updateDetailsView :: DetailsView
                  -> Maybe (Detailed Message)
                  -> IO ()
updateDetailsView detailsView newMessage = do
    case newMessage of
        Nothing -> do
            widgetHide $ detailsViewGetTop detailsView
        Just m  -> do
            detailsViewUpdate detailsView m
            widgetShow $ detailsViewGetTop detailsView

updateDisplayedLog :: MonadIO io
                   => WindowInfo
                   -> RendererResult a
                   -> io ()
updateDisplayedLog wi rr = io $ do
    let shapes = rrShapes rr
        regions = rrRegions rr
        canvas = wiCanvas wi

    (windowWidth, _) <- windowGetSize (wiWindow wi)

    canvasSetShapes canvas shapes regions (rrCentreOffset rr) windowWidth

prettyDirectory :: String
                -> String
prettyDirectory s = "(" ++ dropTrailingPathSeparator s ++ ")"

logWindowTitle :: LogDetails
               -> String
logWindowTitle (RecordedLog filepath) = "(*) " ++ takeFileName filepath
logWindowTitle (SingleLog   filepath) =
    intercalate " " [name, prettyDirectory directory]
  where
    (directory, name) = splitFileName filepath
logWindowTitle (TwoLogs sessionPath systemPath) =
    intercalate " " $ filter (not . null)
           [ sessionName, sessionDirectory'
           , "&"
           , systemName,  prettyDirectory systemDirectory
           ]
  where
    (sessionDirectory, sessionName) = splitFileName sessionPath
    (systemDirectory,  systemName ) = splitFileName systemPath
    sessionDirectory' =
      if sessionDirectory == systemDirectory
        then ""
        else prettyDirectory sessionDirectory

logTitle :: LogDetails
         -> String
logTitle (RecordedLog filepath) = dropExtension $ takeFileName filepath
logTitle (SingleLog   filepath) = dropExtension $ takeFileName filepath
logTitle (TwoLogs sessionPath systemPath) =
    intercalate " & " . map (dropExtension . takeFileName)
                      $ [sessionPath, systemPath]

wiSetLogDetails :: WindowInfo
                -> LogDetails
                -> IO ()
wiSetLogDetails wi logDetails = do
    writeIORef (wiLogDetails wi) (Just logDetails)
    windowSetTitle (wiWindow wi)
        (printf (__ "%s - Bustle") (logWindowTitle logDetails) :: String)

setPage :: MonadIO io
        => WindowInfo
        -> Page
        -> io ()
setPage wi page = io $ notebookSetCurrentPage (wiNotebook wi) (fromEnum page)

displayLog :: WindowInfo
           -> LogDetails
           -> Log
           -> Log
           -> RendererResult Participants
           -> B ()
displayLog wi@(WindowInfo { wiWindow = window
                       , wiExport = exportItem
                       , wiViewStatistics = viewStatistics
                       , wiFilterNames = filterNames
                       , wiCanvas = canvas
                       , wiStatsBook = statsBook
                       , wiStatsPane = statsPane
                       })
           logDetails
           sessionMessages
           systemMessages
           rr = do
  io $ do
    wiSetLogDetails wi logDetails

    hiddenRef <- newIORef Set.empty

    updateDisplayedLog wi rr

    widgetSetSensitivity exportItem True
    onActivateLeaf exportItem $ do
        shapes <- canvasGetShapes canvas
        saveToPDFDialogue wi shapes

    setPage wi CanvasPage
    canvasFocus canvas

    -- FIXME: this currently shows stats for all messages, not post-filtered messages
    statsPaneSetMessages statsPane sessionMessages systemMessages

    widgetSetSensitivity viewStatistics True
    viewStatistics `on` checkMenuItemToggled $ do
        active <- checkMenuItemGetActive viewStatistics
        if active
            then widgetShow statsBook
            else widgetHide statsBook

    widgetSetSensitivity filterNames True
    onActivateLeaf filterNames $ do
        hidden <- readIORef hiddenRef
        hidden' <- runFilterDialog window (sessionParticipants $ rrApplications rr) hidden
        writeIORef hiddenRef hidden'
        let rr' = processWithFilters (sessionMessages, hidden') (systemMessages, Set.empty)

        updateDisplayedLog wi rr'

  return ()

loadPixbuf :: FilePath -> IO (Maybe Pixbuf)
loadPixbuf filename = do
  iconName <- getDataFileName $ "data/" ++ filename
  C.catch (fmap Just (pixbufNewFromFile iconName))
          (\(GError _ _ msg) -> warn (toString msg) >> return Nothing)

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
          makeCallback (loadInInitialWindow (SingleLog fn)) r
      widgetDestroy chooser

  widgetShowAll chooser

saveToPDFDialogue :: WindowInfo
                  -> Diagram
                  -> IO ()
saveToPDFDialogue wi shapes = do
  let parent = Just (wiWindow wi)
  chooser <- fileChooserDialogNew Nothing parent FileChooserActionSave
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-save", ResponseAccept)
             ]
  chooser `set` [ windowModal := True
                , fileChooserLocalOnly := True
                , fileChooserDoOverwriteConfirmation := True
                ]

  Just logDetails <- readIORef $ wiLogDetails wi

  let filename  = logTitle logDetails <.> "pdf"
  fileChooserSetCurrentName chooser filename

  -- If the currently-loaded log has a meaningful directory, suggest that as
  -- the default.
  let mdirectory = case logDetails of
          RecordedLog _ -> Nothing
          SingleLog p   -> Just $ takeDirectory p
          TwoLogs p _   -> Just $ takeDirectory p
  maybeM mdirectory $ fileChooserSetCurrentFolder chooser

  chooser `afterResponse` \resp -> do
      when (resp == ResponseAccept) $ do
          Just fn <- io $ fileChooserGetFilename chooser
          let (width, height) = diagramDimensions shapes
          withPDFSurface fn width height $
            \surface -> renderWith surface $ drawDiagram False shapes
      widgetDestroy chooser

  widgetShowAll chooser

-- vim: sw=2 sts=2
