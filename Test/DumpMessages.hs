module Main where

import System.Environment (getArgs)
import Control.Monad (forM_)

import Bustle.Loader.Pcap (readPcap)

main = do
    args <- getArgs
    let file = case args of
            x:_ -> x
            _   -> error "gimme a filename"
    (Right (warnings, messages)) <- readPcap file
    forM_ (zip [1..] messages) $ \(i, message) ->
        putStrLn $ show i ++ ": " ++ show message
