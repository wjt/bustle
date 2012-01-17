module Main where

import System.Environment (getArgs)
import Control.Monad (forM_)

import Bustle.Loader.Pcap (readPcap)

main = do
    [file] <- getArgs
    (Right (warnings, messages)) <- readPcap file
    forM_ (zip [1..] messages) $ \(i, message) ->
        putStrLn $ show i ++ ": " ++ show message
