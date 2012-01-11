module Bustle.UI.Recorder
  (
    recorderNew
  , recorderRun
  )
where

import Control.Monad (when)
import Control.Concurrent.MVar

import System.Glib.GError
import Graphics.UI.Gtk

import Bustle.Monitor
import Bustle.UI.Util (displayError)

type RecorderCallback = (FilePath -> IO ())

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
            "Logged <b>" ++ show i ++ "</b> messages to <i>" ++ filename ++ "</i>…"
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
        callback filename

    widgetShowAll dialog
  where
    newFailed (GError _ _ message) = do
        displayError mwindow message Nothing

recorderNew :: Maybe Window
            -> RecorderCallback
            -> IO ()
recorderNew mwindow callback = do
    chooser <- fileChooserDialogNew Nothing mwindow FileChooserActionSave
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-new", ResponseAccept)
             ]
    chooser `set` [ windowModal := True
                  , fileChooserLocalOnly := True
                  , fileChooserDoOverwriteConfirmation := True
                  ]

    chooser `afterResponse` \resp -> do
        when (resp == ResponseAccept) $ do
            Just fn <- fileChooserGetFilename chooser
            recorderRun fn mwindow callback
        widgetDestroy chooser

    widgetShowAll chooser
