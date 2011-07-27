import Prelude hiding (log)

import Control.Monad
import Data.List
import Data.Maybe
import System
import Control.Monad.Error

import Bustle.Loader
import Bustle.Types

run :: FilePath -> IO ()
run filepath = do
    ret <- runErrorT $ readLog filepath
    case ret of
        Left (LoadError _ err) ->
            putStrLn $ concat ["Couldn't parse ", filepath, ": ", err]
        Right log -> do
            putStrLn "digraph bustle {"
            forM_ (nub . mapMaybe (methodCall . dmMessage) $ log)
                (\(s, d) -> putStrLn . concat $
                    ["  \"", unBusName s, "\" -> \"", unBusName d, "\";"])
            putStrLn "}"
    where methodCall (MethodCall {sender = s, destination = d}) = Just (s, d)
          methodCall _ = Nothing

main :: IO ()
main = do
   args <- getArgs
   case args of
       [filepath] -> run filepath
       _ -> exitFailure

