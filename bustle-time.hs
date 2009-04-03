import Prelude hiding (log)

import Data.Function
import qualified Data.Map as M
import Data.List
import Data.Maybe
import System
import Text.Printf

import Bustle.Parser (readLog)
import Bustle.Types

mean :: Fractional a => [a] -> a
mean = acc 0 0
   where acc 0 _ [] = error "mean of empty list"
         acc n t [] = t / n
         acc n t (x:xs) = acc (n + 1) (t + x) xs

mkTimes :: [Message] -> [(String, Double, Int, Double)]
mkTimes = map (\(method, (total, times)) ->
                  (method,
                   fromInteger total / 1000,
                   length times,
                   (mean $ map fromInteger times) / 1000))
      . M.toList
      . foldr (\(method, time) -> M.alter (alt time) method) M.empty
      . mapMaybe methodReturn
    where alt newtime Nothing = Just (newtime, [newtime])
          alt newtime (Just (total, times)) =
              Just (newtime + total, newtime : times)

          methodReturn (MethodReturn { timestamp = end,
                            inReplyTo = Just (MethodCall {
                                timestamp = start, member = m }) }) =
              Just (memberStr m, end - start)
          methodReturn _ = Nothing

          memberStr m = iface m ++ "." ++ membername m

run :: FilePath -> IO ()
run filepath = do
    input <- readFile filepath
    case readLog input of
        Left err -> putStrLn $ concat ["Couldn't parse ", filepath, ": ", show err]
        Right log -> mapM_ (\(method, total, ncalls, mean_) ->
                                 printf " %9.4f %3d %9.4f %s\n"
                                     total ncalls mean_ method)
                         . reverse
                         . sortBy (compare `on` (\(_, b, _, _) -> b))
                         . mkTimes $ log

main :: IO ()
main = do
   args <- getArgs
   case args of
       [filepath] -> run filepath
       _ -> exitFailure

