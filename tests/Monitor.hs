{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Bustle.UI.Recorder

import Graphics.UI.Gtk
import System.Glib.GError
import Control.Concurrent.MVar

main = do
    args <- initGUI
    let filename = case args of
            x:xs -> x
            _    -> error "gimme a filename"
    recorderRun filename Nothing mainQuit
    mainGUI
