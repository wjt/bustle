import Prelude hiding (log)

import System
import Text.Printf

import Bustle.Parser (readLog)
import Bustle.Types
import Bustle.Stats (methodTimes)

run :: FilePath -> IO ()
run filepath = do
    input <- readFile filepath
    case readLog input of
        Left err -> putStrLn $ concat ["Couldn't parse ", filepath, ": ", show err]
        Right log -> mapM_ (\(method, total, ncalls, mean_) ->
                                 printf " %9.4f %3d %9.4f %s\n"
                                     total ncalls mean_ method)
                         . methodTimes $ log

main :: IO ()
main = do
   args <- getArgs
   case args of
       [filepath] -> run filepath
       _ -> exitFailure

