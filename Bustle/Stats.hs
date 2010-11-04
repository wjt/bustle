module Bustle.Stats
  ( TallyType(..)
  , FrequencyInfo(..)
  , frequencies
  , methodTimes
  , TimeInfo(..)
  )
where

import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

import qualified Data.Map as M

import Bustle.Types

data TallyType = TallyMethod | TallySignal
    deriving (Eq, Ord, Show)

memberStr :: Member -> String
memberStr m = iface m ++ "." ++ membername m

repr :: Message -> Maybe (TallyType, String)
repr msg =
    case msg of
        MethodCall { member = m } -> Just (TallyMethod, memberStr m)
        Signal     { member = m } -> Just (TallySignal, memberStr m)
        _                         -> Nothing

data FrequencyInfo =
    FrequencyInfo { fiFrequency :: Int
                  , fiType :: TallyType
                  , fiMember :: String
                  }
  deriving (Show, Eq, Ord)

frequencies :: Log -> [FrequencyInfo]
frequencies = reverse
            . sort
            . map (\((t, m), c) -> FrequencyInfo c t m)
            . M.toList
            . foldr (M.alter alt) M.empty
            . mapMaybe repr
    where alt Nothing  = Just 1
          alt (Just n) = Just (n + 1)

mean :: Fractional a => [a] -> a
mean = acc 0 0
   where acc 0 _ [] = error "mean of empty list"
         acc n t [] = t / n
         acc n t (x:xs) = acc (n + 1) (t + x) xs

data TimeInfo =
    TimeInfo { tiMethodName :: String
             , tiTotalTime :: Double -- seconds
             , tiCallFrequency :: Int
             , tiMeanCallTime :: Double -- seconds
             }

methodTimes :: Log -> [TimeInfo]
methodTimes = reverse
            . sortBy (comparing tiTotalTime)
            . map summarize
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

          summarize (method, (total, times)) =
              TimeInfo { tiMethodName = method
                       , tiTotalTime = fromInteger total / 1000
                       , tiCallFrequency = length times
                       , tiMeanCallTime = (mean $ map fromInteger times) / 1000
                       }
