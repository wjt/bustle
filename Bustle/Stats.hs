{-
Bustle.Stats: calculates statistics for D-Bus logs
Copyright © 2009–2011 Collabora Ltd.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}
module Bustle.Stats
  ( TallyType(..)
  , FrequencyInfo(..)
  , frequencies

  , methodTimes
  , TimeInfo(..)

  , messageSizes
  , SizeType(..)
  , SizeInfo(..)
  )
where

import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

import qualified Data.Map as Map
import Data.Map (Map)

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
            . Map.toList
            . foldr (Map.alter alt) Map.empty
            . mapMaybe repr
    where alt Nothing  = Just 1
          alt (Just n) = Just (n + 1)

mean :: (Eq a, Fractional a) => [a] -> a
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
            . Map.toList
            . foldr (\(i, method, time) ->
                        Map.alter (alt time) (i, method)) Map.empty
            . mapMaybe methodReturn
    where alt newtime Nothing = Just (newtime, [newtime])
          alt newtime (Just (total, times)) =
              Just (newtime + total, newtime : times)

          methodReturn :: DetailedMessage
                       -> Maybe (Maybe Interface, MemberName, Microseconds)
          methodReturn dm = case dmMessage dm of
              MethodReturn { inReplyTo =
                                 Just (DetailedMessage start call@(MethodCall {}) _)
                           } -> Just ( iface (member call)
                                     , membername (member call)
                                     , dmTimestamp dm - start
                                     )
              _              -> Nothing

          summarize ((i, method), (total, times)) =
              TimeInfo { tiInterface = i
                       , tiMethodName = method
                       , tiTotalTime = fromIntegral total / 1000
                       , tiCallFrequency = length times
                       , tiMeanCallTime = (mean $ map fromIntegral times) / 1000
                       }

-- FIXME: really? again?
data SizeType = SizeCall
              | SizeReturn
              | SizeError
              | SizeSignal
  deriving
    (Show, Ord, Eq)

-- The fields are in this ideosyncratic order to make the derived Ord instance
-- do what we want
data SizeInfo =
    SizeInfo { siMeanSize, siMaxSize, siMinSize :: Int
             , siType :: SizeType
             , siInterface :: Maybe Interface
             , siName :: MemberName
             }
  deriving
    (Show, Ord, Eq)

messageSizes :: Log
             -> [SizeInfo]
messageSizes messages =
    reverse . sort . map summarize $ Map.assocs sizeTable
  where
    summarize :: ((SizeType, Maybe Interface, MemberName), [Int]) -> SizeInfo
    summarize ((t, i, m), sizes) =
        SizeInfo (intMean sizes) (maximum sizes) (minimum sizes) t i m

    intMean :: [Int] -> Int
    intMean = ceiling . (mean :: [Double] -> Double) . map fromIntegral

    sizeTable = foldr f Map.empty messages

    f :: DetailedMessage -> Map (SizeType, Maybe Interface, MemberName) [Int]
                         -> Map (SizeType, Maybe Interface, MemberName) [Int]
    f dm = case (sizeKeyRepr dm, dmDetails dm) of
        (Just key, Just (size, _)) -> Map.insertWith' (++) key [size]
        _                          -> id

    sizeKeyRepr :: DetailedMessage
                -> Maybe (SizeType, Maybe Interface, MemberName)
    sizeKeyRepr (DetailedMessage _ msg _) =
        case msg of
            MethodCall { member = m } -> Just (SizeCall, iface m, membername m)
            Signal     { member = m } -> Just (SizeSignal, iface m, membername m)
            MethodReturn { inReplyTo = (Just (DetailedMessage _ msg' _)) } ->
                Just (SizeReturn, iface (member msg'), membername (member msg'))
            Error { inReplyTo = (Just (DetailedMessage _ msg' _)) } ->
                Just (SizeError, iface (member msg'), membername (member msg'))
            _                         -> Nothing
