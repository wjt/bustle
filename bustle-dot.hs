import Prelude hiding (log)

import Control.Monad
import Data.List
import Data.Maybe
import System

import Bustle.Parser (readLog)
import Bustle.Types

run :: FilePath -> IO ()
run filepath = do
    input <- readFile filepath
    case readLog input of
        Left err -> putStrLn $ concat ["Couldn't parse ", filepath, ": ", show err]
        Right log -> do
            putStrLn "digraph bustle {"
            forM_ (nub . mapMaybe methodCall $ log)
                (\(s, d) -> putStrLn . concat $
                    ["  \"", s, "\" -> \"", d, "\";"])
            putStrLn "}"
    where methodCall (MethodCall {sender = s, destination = d}) = Just (s, d)
          methodCall _ = Nothing

main :: IO ()
main = do
   args <- getArgs
   case args of
       [filepath] -> run filepath
       _ -> exitFailure

