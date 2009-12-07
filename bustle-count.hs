import Prelude hiding (log)

import System
import Text.Printf

import Bustle.Parser (readLog)
import Bustle.Stats (frequencies)

run :: FilePath -> IO ()
run filepath = do
    input <- readFile filepath
    case readLog input of
        Left err -> putStrLn $ concat ["Couldn't parse ", filepath, ": ", show err]
        Right log -> mapM_ (\(c, (t, s)) -> printf " %4d %6s %s\n" c (show t) s)
                         . frequencies $ log

main :: IO ()
main = do
   args <- getArgs
   case args of
       [filepath] -> run filepath
       _ -> exitFailure

