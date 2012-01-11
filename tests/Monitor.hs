{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Bustle.Monitor

import Graphics.UI.Gtk
import System.Glib.GError
import Control.Concurrent.MVar

main = do
    args <- initGUI
    let filename = case args of
            x:xs -> x
            _    -> error "gimme a filename"
    emonitor <- monitorNew BusTypeSession filename DebugOutput
    case emonitor of
        Left (GError _ _ message) -> error message
        Right monitor -> do

            window <- windowNew

            label <- labelNew Nothing
            let updateLabel n = labelSetMarkup label $
                    "Logged <b>" ++ show n ++ "</b> messages to <i>" ++ filename ++ "</i>…"
            n <- newMVar 0
            updateLabel 0
            monitor `on` monitorMessageLogged $ do
                i <- takeMVar n
                let j = i + 1 :: Integer
                updateLabel j
                putMVar n j

            bar <- progressBarNew
            id <- timeoutAdd (progressBarPulse bar >> return True) 100

            vbox <- vBoxNew False 6
            boxPackStart vbox label PackGrow 0
            boxPackStart vbox bar PackNatural 0

            set window [ containerBorderWidth := 6
                       , containerChild := vbox
                       ]
            onDestroy window mainQuit
            widgetShowAll window

            mainGUI

            monitorStop monitor
