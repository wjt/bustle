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

import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Data.Maybe (isNothing)
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
import Bustle.UI.AboutDialog
import Bustle.UI.DetailsView
import Bustle.UI.FilterDialog
import Bustle.UI.OpenTwoDialog (setupOpenTwoDialog)
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

import System.FilePath ( splitFileName, takeFileName, takeDirectory
                       , replaceExtension, (</>), (<.>)
                       )
import System.Directory (renameFile)

import qualified DBus.Message

type B a = Bustle BConfig BState a

data LogDetails =
    LogDetails { ldSessionPath :: FilePath
               , ldSystemPath :: Maybe FilePath
               }

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

               , wiLogDetails :: IORef (Maybe LogDetails)
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
        let logDetails = LogDetails { ldSessionPath = session
                                    , ldSystemPath = maybeSystem
                                    }
        lift $ displayLog windowInfo
                          logDetails
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

  -- Open two logs dialog
  openTwoDialog <- embedIO $ \r ->
      setupOpenTwoDialog xml window $ \f1 f2 ->
          makeCallback (loadInInitialWindow f1 (Just f2)) r
  withProgramIcon (windowSetIcon openTwoDialog)

  -- Set up the window itself
  withProgramIcon (windowSetIcon window)
  embedIO $ onDestroy window . makeCallback maybeQuit

  -- File menu
  embedIO $ onActivateLeaf newItem . makeCallback startRecording
  embedIO $ onActivateLeaf openItem . makeCallback (openDialogue window)
  io $ openTwoItem `onActivateLeaf` widgetShowAll openTwoDialog
  io $ closeItem `onActivateLeaf` widgetDestroy window

  -- Help menu
  withProgramIcon $ \icon -> io $
      onActivateLeaf aboutItem $ showAboutDialog window icon

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
                              , wiLayout = layout
                              , wiDetailsView = details
                              , wiClampIdleId = clampIdleId
                              , wiLogDetails = logDetailsRef
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

logTitle :: LogDetails
         -> String
logTitle logDetails =
    case ldSystemPath logDetails of
        Nothing         -> sessionName
        Just systemPath -> takeFileName systemPath ++ " & " ++ sessionName
  where
    sessionName = takeFileName $ ldSessionPath logDetails

wiSetLogDetails :: WindowInfo
                -> LogDetails
                -> IO ()
wiSetLogDetails wi logDetails = do
    writeIORef (wiLogDetails wi) (Just logDetails)
    windowSetTitle (wiWindow wi) (logTitle logDetails ++ " — Bustle")

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
                       , wiLayout = layout
                       , wiNotebook = nb
                       , wiStatsBook = statsBook
                       , wiStatsPane = statsPane
                       })
           logDetails
           sessionMessages
           systemMessages
           rr = do
  showBounds <- asks debugEnabled

  io $ do
    wiSetLogDetails wi logDetails

    shapesRef <- newIORef []
    widthRef <- newIORef 0
    regionSelectionRef <- newIORef $ regionSelectionNew []
    hiddenRef <- newIORef Set.empty

    updateDisplayedLog wi rr shapesRef widthRef regionSelectionRef

    widgetSetSensitivity exportItem True
    onActivateLeaf exportItem $ do
        shapes <- readIORef shapesRef
        saveToPDFDialogue wi shapes

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

withProgramIcon :: (Maybe Pixbuf -> IO a) -> B a
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

  let directory = takeDirectory $ ldSessionPath logDetails
      filename  = replaceExtension (logTitle logDetails) "pdf"

  fileChooserSetCurrentFolder chooser directory
  fileChooserSetCurrentName chooser filename

  chooser `afterResponse` \resp -> do
      when (resp == ResponseAccept) $ do
          Just fn <- io $ fileChooserGetFilename chooser
          let (width, height) = diagramDimensions shapes
          withPDFSurface fn width height $
            \surface -> renderWith surface $ drawDiagram False shapes
      widgetDestroy chooser

  widgetShowAll chooser

-- vim: sw=2 sts=2
