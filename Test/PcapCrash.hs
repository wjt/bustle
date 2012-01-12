-- A regression test for <https://bugs.freedesktop.org/show_bug.cgi?id=44714>.
--
-- log-with-h.bustle is a log file containing a file handle. This used to make
-- readPcap call 'error'.
module Main where

import System.Exit (exitFailure)
import Bustle.Loader.Pcap (readPcap)

path = "Test/data/log-with-h.bustle"

main = do
    ret <- readPcap path
    case ret of
        Left e -> do
            putStrLn $ "Failed to read '" ++ path ++ "': " ++ show e
            exitFailure
        -- TODO: check there are no warnings (but there are because we don't
        -- understand 'h', so we just skip it)
        Right _ -> do
            return ()
