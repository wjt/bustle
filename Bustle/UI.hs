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

import Prelude hiding (catch)

import Control.Exception
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Data.Maybe (isJust, isNothing, fromJust, listToMaybe)
import Data.Version (showVersion)
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time

import Paths_bustle
import Bustle.Application.Monad
import Bustle.Renderer
import Bustle.Types
import Bustle.Diagram
import Bustle.Regions
import Bustle.Util
import Bustle.UI.DetailsView
import Bustle.UI.FilterDialog
import Bustle.UI.Recorder
import Bustle.UI.Util (displayError)
import Bustle.StatisticsPane
import Bustle.Loader

import System.Glib.GError (GError(..), catchGError)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.DrawWindow (drawWindowInvalidateRect)
import Graphics.Rendering.Pango.Structs (Rectangle)

import Graphics.Rendering.Cairo (withPDFSurface, renderWith)

import System.FilePath (splitFileName, takeFileName, replaceExtension, (</>), (<.>))
import System.Directory (renameFile)

import qualified DBus.Message

type B a = Bustle BConfig BState a

data WindowInfo =
    WindowInfo { wiWindow :: Window
               , wiSave :: ImageMenuItem
               , wiExport :: ImageMenuItem
               , wiViewStatistics :: CheckMenuItem
               , wiFilterNames :: MenuItem
               , wiNotebook :: Notebook
               , wiStatsBook :: Notebook
               , wiStatsPane :: StatsPane
               , wiContentVPaned :: VPaned
               , wiLayout :: Layout
               , wiDetailsView :: DetailsView
               , wiClampIdleId :: IORef (Maybe HandlerId)
               }

data BConfig =
    BConfig { debugEnabled :: Bool
            , bustleIcon :: Maybe Pixbuf
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
uiMain = do
    args <- initGUI

    -- FIXME: get a real option parser
    let debug = any isDebug args

    [bustle, method, signal] <- mapM loadPixbuf
        ["bustle.png", "dfeet-method.png", "dfeet-signal.png"]

    let config = BConfig { debugEnabled = debug
                         , bustleIcon = bustle
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

consumeInitialWindow :: B WindowInfo
consumeInitialWindow = do
    x <- gets initialWindow
    case x of
        Nothing   -> emptyWindow
        Just windowInfo -> do
            modify $ \s -> s { initialWindow = Nothing }
            return windowInfo

loadInInitialWindow :: FilePath -> Maybe FilePath -> B ()
loadInInitialWindow = loadLogWith consumeInitialWindow

loadLog :: FilePath -> Maybe FilePath -> B ()
loadLog = loadLogWith emptyWindow

loadLogWith :: B WindowInfo   -- ^ action returning a window to load the log(s) in
            -> FilePath       -- ^ a log file to load and display
            -> Maybe FilePath -- ^ an optional second log to show alongside the
                              --   first log.
            -> B ()
loadLogWith getWindow session maybeSystem = do
    ret <- runErrorT $ do
        (sessionWarnings, sessionMessages) <- readLog session
        (systemWarnings, systemMessages) <- case maybeSystem of
            Just system -> readLog system
            Nothing     -> return ([], [])

        -- FIXME: pass the log file name into the renderer
        let rr = process sessionMessages systemMessages
        io $ mapM warn $ sessionWarnings ++ systemWarnings ++ rrWarnings rr

        windowInfo <- lift getWindow
        lift $ displayLog windowInfo
                          session
                          maybeSystem
                          sessionMessages
                          systemMessages
                          rr

    case ret of
      Left (LoadError f e) -> io $
          displayError Nothing ("Could not read '" ++ f ++ "'") (Just e)
      Right () -> return ()

startRecording :: B ()
startRecording = do
    wi <- consumeInitialWindow

    zt <- io $ getZonedTime
    -- I hate time manipulation
    let yyyy_mm_dd_hh_mm_ss = takeWhile (/= '.') (show zt)

    cacheDir <- io $ getCacheDir
    let filename = cacheDir </> yyyy_mm_dd_hh_mm_ss <.> "bustle"

    embedIO $ \r -> recorderRun filename (Just (wiWindow wi)) $
          makeCallback (finishedRecording wi filename) r

finishedRecording :: WindowInfo
                  -> FilePath
                  -> B ()
finishedRecording wi tempFilePath = do
    loadLogWith (return wi) tempFilePath Nothing

    let saveItem     = wiSave wi
        mwindow      = Just (wiWindow wi)
        tempFileName = takeFileName tempFilePath

    io $ do
        widgetSetSensitivity saveItem True
        onActivateLeaf saveItem $ do
            recorderChooseFile tempFileName mwindow $ \newFilePath -> do
                renameFile tempFilePath newFilePath
                widgetSetSensitivity saveItem False
    return ()

maybeQuit :: B ()
maybeQuit = do
  n <- decWindows
  when (n == 0) (io mainQuit)

emptyWindow :: B WindowInfo
emptyWindow = do
  Just xml <- io $ xmlNew =<< getDataFileName "data/bustle.glade"

  -- Grab a bunch of widgets. Surely there must be a better way to do this?
  let getW cast name = io $ xmlGetWidget xml cast name

  window <- getW castToWindow "diagramWindow"
  [newItem, openItem, saveItem, exportItem, closeItem, aboutItem] <-
      mapM (getW castToImageMenuItem)
          ["new", "open", "save", "export", "close", "about"]
  openTwoItem <- getW castToMenuItem "openTwo"
  viewStatistics <- getW castToCheckMenuItem "statistics"
  filterNames <- getW castToMenuItem "filter"
  layout <- getW castToLayout "diagramLayout"
  [nb, statsBook] <- mapM (getW castToNotebook)
      ["diagramOrNot", "statsBook"]
  contentVPaned <- getW castToVPaned "contentVPaned"

  -- Open two logs dialog widgets
  openTwoDialog <- getW castToDialog "openTwoDialog"
  [sessionBusChooser, systemBusChooser] <- mapM (getW castToFileChooserButton)
      ["sessionBusChooser", "systemBusChooser"]

  -- Set up the window itself
  withProgramIcon (windowSetIcon window)
  embedIO $ onDestroy window . makeCallback maybeQuit

  -- File menu
  embedIO $ onActivateLeaf newItem . makeCallback startRecording
  embedIO $ onActivateLeaf openItem . makeCallback (openDialogue window)
  io $ openTwoItem `onActivateLeaf` widgetShowAll openTwoDialog
  io $ closeItem `onActivateLeaf` widgetDestroy window

  -- Help menu
  embedIO $ onActivateLeaf aboutItem . makeCallback (showAbout window)

  -- Diagram area panning
  io $ do
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

  -- Open two logs dialog
  withProgramIcon (windowSetIcon openTwoDialog)

  io $ do
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

  m <- asks methodIcon
  s <- asks signalIcon
  statsPane <- io $ statsPaneNew xml m s

  details <- io $ detailsViewNew
  io $ do
      let top = detailsViewGetTop details
      panedPack2 contentVPaned top False False
      -- Hide the details by default; they'll be shown when the user selects a
      -- message.
      widgetHide top

  clampIdleId <- io $ newIORef Nothing
  let windowInfo = WindowInfo { wiWindow = window
                              , wiSave = saveItem
                              , wiExport = exportItem
                              , wiViewStatistics = viewStatistics
                              , wiFilterNames = filterNames
                              , wiNotebook = nb
                              , wiStatsBook = statsBook
                              , wiStatsPane = statsPane
                              , wiContentVPaned = contentVPaned
                              , wiLayout = layout
                              , wiDetailsView = details
                              , wiClampIdleId = clampIdleId
                              }

  incWindows
  io $ widgetShow window
  return windowInfo

invalidateRect :: Layout
               -> Stripe
               -> IO ()
invalidateRect layout (Stripe y1 y2) = do
    win <- layoutGetDrawWindow layout
    (width, _height) <- layoutGetSize layout
    let pangoRectangle = Rectangle 0 (floor y1) width (ceiling y2)

    drawWindowInvalidateRect win pangoRectangle False

type RSDM = RegionSelection DetailedMessage
queueClampAroundSelection :: IORef RSDM
                          -> WindowInfo
                          -> IO ()
queueClampAroundSelection regionSelectionRef wi = do
    let idRef = wiClampIdleId wi
    id_ <- readIORef idRef
    when (isNothing id_) $ do
        id' <- flip idleAdd priorityDefaultIdle $ do
            rs <- readIORef regionSelectionRef
            case rsCurrent rs of
                Nothing -> return ()
                Just (Stripe top bottom, _) -> do
                    vadj <- layoutGetVAdjustment (wiLayout wi)
                    let padding = (bottom - top) / 2
                    adjustmentClampPage vadj (top - padding) (bottom + padding)

            writeIORef idRef Nothing
            return False

        writeIORef idRef (Just id')

modifyRegionSelection :: IORef RSDM
                      -> WindowInfo
                      -> (RSDM -> RSDM)
                      -> IO ()
modifyRegionSelection regionSelectionRef wi f = do
    let layout      = wiLayout wi
        detailsView = wiDetailsView wi

    rs <- readIORef regionSelectionRef
    let currentMessage = rsCurrent rs
        rs' = f rs
        newMessage = rsCurrent rs'
    writeIORef regionSelectionRef rs'

    when (newMessage /= currentMessage) $ do
        case newMessage of
            Nothing     -> do
                widgetHide $ detailsViewGetTop detailsView
            Just (r, m) -> do
                detailsViewUpdate detailsView m
                invalidateRect layout r
                widgetShow $ detailsViewGetTop detailsView
                queueClampAroundSelection regionSelectionRef wi

        case currentMessage of
            Nothing -> return ()
            Just (r, _) -> invalidateRect layout r

updateDisplayedLog :: WindowInfo
                   -> RendererResult a
                   -> IORef [Shape]
                   -> IORef Double
                   -> IORef (RegionSelection DetailedMessage)
                   -> IO ()
updateDisplayedLog wi rr shapesRef widthRef regionSelectionRef = do
    let shapes = rrShapes rr
        (width, height) = diagramDimensions shapes

        layout = wiLayout wi

    writeIORef shapesRef shapes
    writeIORef widthRef width

    modifyRegionSelection regionSelectionRef wi $ \rs ->
      let
        rs' = regionSelectionNew (rrRegions rr)
      in
        case rsCurrent rs of
            Just (_, x) -> regionSelectionSelect x rs'
            Nothing     -> rs'

    layoutSetSize layout (floor width) (floor height)

    -- FIXME: only do this the first time maybe?
    -- Shift to make the timestamp column visible
    hadj <- layoutGetHAdjustment layout
    (windowWidth, _) <- windowGetSize (wiWindow wi)
    -- Roughly centre the timestamp-and-member column
    adjustmentSetValue hadj
        ((rrCentreOffset rr) -
            (fromIntegral windowWidth - timestampAndMemberWidth) / 2
        )

    return ()

displayLog :: WindowInfo
           -> FilePath
           -> Maybe FilePath
           -> Log
           -> Log
           -> RendererResult Participants
           -> B ()
displayLog wi@(WindowInfo { wiWindow = window
                       , wiExport = exportItem
                       , wiViewStatistics = viewStatistics
                       , wiFilterNames = filterNames
                       , wiLayout = layout
                       , wiNotebook = nb
                       , wiStatsBook = statsBook
                       , wiStatsPane = statsPane
                       })
           sessionPath
           maybeSystemPath
           sessionMessages
           systemMessages
           rr = do
  let (directory, sessionName) = splitFileName sessionPath
      title = case maybeSystemPath of
          Nothing -> sessionName
          Just systemPath -> takeFileName systemPath ++ " & " ++ sessionName

  showBounds <- asks debugEnabled

  io $ do
    shapesRef <- newIORef []
    widthRef <- newIORef 0
    regionSelectionRef <- newIORef $ regionSelectionNew []
    hiddenRef <- newIORef Set.empty

    updateDisplayedLog wi rr shapesRef widthRef regionSelectionRef

    windowSetTitle window $ title ++ " — Bustle"
    widgetSetSensitivity exportItem True
    onActivateLeaf exportItem $ do
        shapes <- readIORef shapesRef
        saveToPDFDialogue window directory title shapes

    -- I think we could speed things up by only showing the revealed area
    -- rather than everything that's visible.
    layout `on` exposeEvent $ tryEvent $ io $ do
        rs <- readIORef regionSelectionRef
        shapes <- readIORef shapesRef
        width <- readIORef widthRef
        let shapes' =
                case rsCurrent rs of
                    Nothing     -> shapes
                    Just (Stripe y1 y2, _) -> Highlight (0, y1, width, y2):shapes
        update layout shapes' showBounds

    let modifyRS :: MonadIO io
                 => (RSDM -> RSDM)
                 -> io ()
        modifyRS = io . modifyRegionSelection regionSelectionRef wi

    layout `on` buttonPressEvent $ tryEvent $ do
      io $ layout `set` [ widgetIsFocus := True ]
      LeftButton <- eventButton
      (_, y) <- eventCoordinates

      modifyRS (regionSelectionUpdate y)

    layout `on` keyPressEvent $ tryEvent $ do
      [] <- eventModifier
      key <- eventKeyName
      case key of
        "Up"        -> modifyRS regionSelectionUp
        "Down"      -> modifyRS regionSelectionDown
        "Home"      -> modifyRS regionSelectionFirst
        "End"       -> modifyRS regionSelectionLast
        _           -> stopEvent

    notebookSetCurrentPage nb 1
    layout `set` [ widgetIsFocus := True ]

    -- FIXME: this currently shows stats for all messages, not post-filtered messages
    statsPaneSetMessages statsPane sessionMessages systemMessages

    widgetSetSensitivity viewStatistics True
    -- the version of gtk2hs I'm using has a checkMenuItemToggled which is a
    -- method not a signal.
    connectGeneric "toggled" False viewStatistics $ do
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

        updateDisplayedLog wi rr' shapesRef widthRef regionSelectionRef

    -- The stats start off hidden.
    widgetHide statsBook

  return ()

update :: Layout -> Diagram -> Bool -> IO ()
update layout shapes showBounds = do
  win <- layoutGetDrawWindow layout

  hadj <- layoutGetHAdjustment layout
  hpos <- adjustmentGetValue hadj
  hpage <- adjustmentGetPageSize hadj

  vadj <- layoutGetVAdjustment layout
  vpos <- adjustmentGetValue vadj
  vpage <- adjustmentGetPageSize vadj

  let r = (hpos, vpos, hpos + hpage, vpos + vpage)

  renderWithDrawable win $ drawRegion r showBounds shapes

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

withProgramIcon :: (Maybe Pixbuf -> IO ()) -> B ()
withProgramIcon f = asks bustleIcon >>= io . f

loadPixbuf :: FilePath -> IO (Maybe Pixbuf)
loadPixbuf filename = do
  iconName <- getDataFileName $ "data/" ++ filename
  (fmap Just (pixbufNewFromFile iconName)) `catchGError`
    \(GError _ _ msg) -> warn msg >> return Nothing

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

saveToPDFDialogue :: Window
                  -> FilePath
                  -> String
                  -> Diagram
                  -> IO ()
saveToPDFDialogue window directory filename shapes = do
  chooser <- fileChooserDialogNew Nothing (Just window) FileChooserActionSave
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-save", ResponseAccept)
             ]
  chooser `set` [ windowModal := True
                , fileChooserLocalOnly := True
                , fileChooserDoOverwriteConfirmation := True
                ]

  fileChooserSetCurrentFolder chooser directory
  fileChooserSetCurrentName chooser $ replaceExtension filename "pdf"

  chooser `afterResponse` \resp -> do
      when (resp == ResponseAccept) $ do
          Just fn <- io $ fileChooserGetFilename chooser
          let (width, height) = diagramDimensions shapes
          withPDFSurface fn width height $
            \surface -> renderWith surface $ drawDiagram False shapes
      widgetDestroy chooser

  widgetShowAll chooser

showAbout :: Window -> B ()
showAbout window = withProgramIcon $ \icon -> io $ do
    dialog <- aboutDialogNew

    license <- (Just `fmap` (readFile =<< getDataFileName "LICENSE"))
               `catch` (\e -> warn (show (e :: IOException)) >> return Nothing)

    dialog `set` [ aboutDialogName := "Bustle"
                 , aboutDialogVersion := showVersion version
                 , aboutDialogComments := "Someone's favourite D-Bus profiler"
                 , aboutDialogWebsite := "http://willthompson.co.uk/bustle"
                 , aboutDialogAuthors := authors
                 , aboutDialogCopyright := "© 2008–2012 Collabora Ltd."
                 , aboutDialogLicense := license
                 ]
    dialog `afterResponse` \resp ->
        when (resp == ResponseCancel) (widgetDestroy dialog)
    windowSetTransientFor dialog window
    windowSetModal dialog True
    aboutDialogSetLogo dialog icon

    widgetShowAll dialog

authors :: [String]
authors = [ "Will Thompson <will.thompson@collabora.co.uk>"
          , "Dafydd Harries"
          , "Chris Lamb"
          , "Marc Kleine-Budde"
          ]

-- vim: sw=2 sts=2
