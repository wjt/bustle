import Prelude hiding (log)

import qualified Data.Map as M
import Data.List
import Data.Maybe
import System
import Text.Printf

import Bustle.Parser (readLog)
import Bustle.Types

str :: Message -> Maybe ([Char], [Char])
str msg =
    case msg of
        MethodCall { member = m } -> Just ("method", memberStr m)
        Signal     { member = m } -> Just ("signal", memberStr m)
        _                         -> Nothing
    where memberStr m = iface m ++ "." ++ membername m

frequencies :: [Message] -> [(Int, (String, String))]
frequencies = reverse
            . sort
            . map (\(s, c) -> (c, s))
            . M.toList
            . foldr (M.alter alt) M.empty
            . mapMaybe str
    where alt Nothing  = Just 1
          alt (Just n) = Just (n + 1)

run :: FilePath -> IO ()
run filepath = do
    input <- readFile filepath
    case readLog input of
        Left err -> putStrLn $ concat ["Couldn't parse ", filepath, ": ", show err]
        Right log -> mapM_ (\(c, (t, s)) -> printf " %4d %6s %s\n" c t s)
                         . frequencies $ log

main :: IO ()
main = do
   args <- getArgs
   case args of
       [filepath] -> run filepath
       _ -> exitFailure

