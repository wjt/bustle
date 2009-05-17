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
-- lacking them.
upgrade :: Log -> Log
upgrade ms | any isNameOwnerChanged ms = ms
           | otherwise = concat $ evalState (mapM synthesiseNOC ms) Set.empty

synthesiseNOC :: Message -> State (Set BusName) [Message]
synthesiseNOC m | isNameOwnerChanged m = error "guarded above"
synthesiseNOC m@(Signal {sender = n}) = (++ [m]) `fmap` synth n (timestamp m)
synthesiseNOC m = do
    f1 <- synth (sender m) (timestamp m)
    f2 <- synth (destination m) (timestamp m)
    return ( f1 ++ f2 ++ [m] )

synth :: BusName -> Milliseconds -> State (Set BusName) [Message]
synth n ts = do
    b <- gets (Set.member n)
    if b
      then return []
      else do
        modify (Set.insert n)
        return $ case n of
          U u -> [ Connected ts u ]
          O o -> [ Connected ts (fake o)
                 , NameChanged ts o (Claimed (fake o))
                 ]

fake :: OtherName -> UniqueName
fake = UniqueName . (":fake." ++) . unOtherName

-- vim: sw=2 sts=2
