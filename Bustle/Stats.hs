module Bustle.Stats
  ( TallyType
  , frequencies
  )
where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Bustle.Types

data TallyType = TallyMethod | TallySignal
    deriving (Eq, Ord, Show)

repr :: Message -> Maybe (TallyType, String)
repr msg =
    case msg of
        MethodCall { member = m } -> Just (TallyMethod, memberStr m)
        Signal     { member = m } -> Just (TallySignal, memberStr m)
        _                         -> Nothing
    where memberStr m = iface m ++ "." ++ membername m

frequencies :: Log -> [(Int, (TallyType, String))]
frequencies = reverse
            . sort
            . map (\(s, c) -> (c, s))
            . M.toList
            . foldr (M.alter alt) M.empty
            . mapMaybe repr
    where alt Nothing  = Just 1
          alt (Just n) = Just (n + 1)
