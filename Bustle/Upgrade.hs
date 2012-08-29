{-# LANGUAGE OverloadedStrings #-}
{-
Bustle.Upgrade: synthesise information missing from old logs
Copyright (C) 2009 Collabora Ltd.

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
module Bustle.Upgrade (upgrade) where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set

import Bustle.Types

-- Bustle <0.2.0 did not log NameOwnerChanged; this adds fake ones to logs
-- lacking them
upgrade :: [DetailedEvent] -> [DetailedEvent]
upgrade es =
    case partitionDetaileds es of
        ([], ms) -> concat $ evalState (mapM synthesiseNOC ms) Set.empty
        _        -> es

synthesiseNOC :: Detailed Message -> State (Set TaggedBusName) [DetailedEvent]
synthesiseNOC de@(Detailed µs m _) = do
    fakes <- mapM synthDM $ mentionedNames m
    return ( concat fakes ++ [fmap MessageEvent de] )
  where
    synthDM :: TaggedBusName -> State (Set TaggedBusName) [DetailedEvent]
    synthDM n = do
        fakes <- synth n
        return $ map (\fake -> Detailed µs (NOCEvent fake) Nothing) fakes

synth :: TaggedBusName
      -> State (Set TaggedBusName) [NOC]
synth n = do
    b <- gets (Set.member n)
    if b
      then return []
      else do
        modify (Set.insert n)
        return $ case n of
          U u -> [ Connected u ]
          O o -> [ Connected (fakeName o)
                 , NameChanged o (Claimed (fakeName o))
                 ]

fakeName :: OtherName -> UniqueName
fakeName = fakeUniqueName . unOtherName

-- vim: sw=2 sts=2
