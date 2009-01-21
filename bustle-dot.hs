
import Control.Monad
import Data.List
import Data.Maybe
import System

import Bustle.Parser (readLog)
import Bustle.Types

run path = do
    input <- readFile path
    case readLog input of
        Left err -> putStrLn $ concat ["Couldn't parse ", path, ": ", show err]
        Right log -> do
            putStrLn "digraph bustle {"
            forM_ (nub . mapMaybe methodCall $ log)
                (\(s, d) -> putStrLn . concat $
                    ["  \"", s, "\" -> \"", d, "\";"])
            putStrLn "}"
    where methodCall (MethodCall {sender = s, destination = d}) = Just (s, d)
          methodCall _ = Nothing

main = do
   args <- getArgs
   case args of
       [path] -> run path
       _ -> exitFailure

