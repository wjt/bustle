module Bustle.UI.Recorder
  (
    recorderChooseFile
  , recorderRun
  )
where

import Control.Monad (when)
import Control.Concurrent.MVar

import System.Glib.GError
import Graphics.UI.Gtk

import Bustle.Monitor
import Bustle.UI.Util (displayError)

type RecorderCallback = IO ()

recorderRun :: FilePath
            -> Maybe Window
            -> RecorderCallback
            -> IO ()
recorderRun filename mwindow callback = handleGError newFailed $ do
    monitor <- monitorNew BusTypeSession filename NoDebugOutput
    dialog <- dialogNew

    maybe (return ()) (windowSetTransientFor dialog) mwindow
    dialog `set` [ windowModal := True ]

    label <- labelNew Nothing
    n <- newMVar (0 :: Integer)
    let updateLabel = do
        i <- takeMVar n
        let j = i + 1
        labelSetMarkup label $
            "Logged <b>" ++ show i ++ "</b> messages…"
        putMVar n j
    handlerId <- monitor `on` monitorMessageLogged $ updateLabel
    updateLabel

    bar <- progressBarNew
    pulseId <- timeoutAdd (progressBarPulse bar >> return True) 100

    vbox <- dialogGetUpper dialog
    boxPackStart vbox label PackGrow 0
    boxPackStart vbox bar PackNatural 0

    dialogAddButton dialog "gtk-media-stop" ResponseClose

    dialog `afterResponse` \_ -> do
        monitorStop monitor
        signalDisconnect handlerId
        timeoutRemove pulseId
        widgetDestroy dialog
        callback

    widgetShowAll dialog
  where
    newFailed (GError _ _ message) = do
        displayError mwindow message Nothing

recorderChooseFile :: FilePath
                   -> Maybe Window
                   -> (FilePath -> IO ())
                   -> IO ()
recorderChooseFile name mwindow callback = do
    chooser <- fileChooserDialogNew Nothing mwindow FileChooserActionSave
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-new", ResponseAccept)
             ]
    fileChooserSetCurrentName chooser name
    chooser `set` [ windowModal := True
                  , fileChooserLocalOnly := True
                  , fileChooserDoOverwriteConfirmation := True
                  ]

    chooser `afterResponse` \resp -> do
        when (resp == ResponseAccept) $ do
            Just fn <- fileChooserGetFilename chooser
            callback fn
        widgetDestroy chooser

    widgetShowAll chooser
