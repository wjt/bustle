module Bustle.Noninteractive
  ( run
  , formatInterface
  )
where

import Prelude hiding (log)

import System
import System.IO (hPutStrLn, stderr)
import Data.Maybe (fromMaybe)
import Control.Monad.Error

import Bustle.Loader
import Bustle.Types

warn :: String -> IO ()
warn = hPutStrLn stderr

process :: FilePath -> (Log -> [a]) -> (a -> String) -> IO ()
process filepath analyze format = do
    ret <- runErrorT $ readLog filepath
    case ret of
        Left (LoadError _ err) -> do
            warn $ concat [ "Couldn't parse "
                          , filepath
                          , ": "
                          , err
                          ]
            exitFailure
        Right log -> mapM_ (putStrLn . format) $ analyze log

run :: String -> (Log -> [a]) -> (a -> String) -> IO ()
run appName analyze format = do
    args <- getArgs
    case args of
        [filepath] -> process filepath analyze format
        _          -> do hPutStrLn stderr $ concat [ "Usage: "
                                                   , appName
                                                   , " foo.bustle"
                                                   ]
                         exitFailure

formatInterface :: Maybe String -> String
formatInterface = fromMaybe "(no interface)"
