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

repr :: DetailedMessage
     -> Maybe (TallyType, Maybe Interface, MemberName)
repr (DetailedMessage _ msg _) =
    case msg of
        MethodCall { member = m } -> Just (TallyMethod, iface m, membername m)
        Signal     { member = m } -> Just (TallySignal, iface m, membername m)
        _                         -> Nothing

data FrequencyInfo =
    FrequencyInfo { fiFrequency :: Int
                  , fiType :: TallyType
                  , fiInterface :: Maybe Interface
                  , fiMember :: MemberName
                  }
  deriving (Show, Eq, Ord)

frequencies :: Log -> [FrequencyInfo]
frequencies = reverse
            . sort
            . map (\((t, i, m), c) -> FrequencyInfo c t i m)
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
    TimeInfo { tiInterface :: Maybe Interface
             , tiMethodName :: MemberName
             , tiTotalTime :: Double -- milliseconds
             , tiCallFrequency :: Int
             , tiMeanCallTime :: Double -- milliseconds
             }

methodTimes :: Log
            -> [TimeInfo]
methodTimes = reverse
            . sortBy (comparing tiTotalTime)
            . map summarize
            . M.toList
            . foldr (\(i, method, time) ->
                        M.alter (alt time) (i, method)) M.empty
            . mapMaybe methodReturn
    where alt newtime Nothing = Just (newtime, [newtime])
          alt newtime (Just (total, times)) =
              Just (newtime + total, newtime : times)

          methodReturn :: DetailedMessage
                       -> Maybe (Maybe Interface, MemberName, Microseconds)
          methodReturn dm = case dmMessage dm of
              MethodReturn { inReplyTo =
                                 Just (DetailedMessage end call@(MethodCall {}) _)
                           } -> Just ( iface (member call)
                                     , membername (member call)
                                     , end - dmTimestamp dm
                                     )
              _              -> Nothing

          summarize ((i, method), (total, times)) =
              TimeInfo { tiInterface = i
                       , tiMethodName = method
                       , tiTotalTime = fromIntegral total / 1000
                       , tiCallFrequency = length times
                       , tiMeanCallTime = (mean $ map fromIntegral times) / 1000
                       }
