{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Bustle.Monitor

import System.Glib.GError
import System.Glib.MainLoop

foreign import ccall "g_type_init" g_type_init :: IO ()

main = do
    g_type_init
    emonitor <- monitorNew BusTypeSession "/tmp/test.bustle" DebugOutput
    case emonitor of
        Left (GError _ _ message) -> error message
        Right monitor -> do
            loop <- mainLoopNew Nothing False
            timeoutAdd (mainLoopQuit loop >> return False) 5000
            mainLoopRun loop
            monitorStop monitor
