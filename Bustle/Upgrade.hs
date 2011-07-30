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
upgrade :: [DetailedMessage] -> [DetailedMessage]
upgrade ms =
        if any (isNameOwnerChanged . dmMessage) ms
            then ms
            else concat $ evalState (mapM synthesiseNOC ms) Set.empty

synthesiseNOC :: DetailedMessage -> State (Set BusName) [DetailedMessage]
synthesiseNOC dm | isNameOwnerChanged (dmMessage dm) = error "guarded above"
synthesiseNOC dm = case m of
    Signal {sender = n} -> do
        fakes <- synthDM n
        return ( fakes ++ [dm] )
    _                   -> do
        f1 <- synthDM (sender m)
        f2 <- synthDM (destination m)
        return ( f1 ++ f2 ++ [dm] )
  where
    m = dmMessage dm
    µs = dmTimestamp dm
    synthDM :: BusName -> State (Set BusName) [DetailedMessage]
    synthDM n = do
        fakes <- synth n
        return $ map (\fake -> DetailedMessage µs fake Nothing) fakes

synth :: BusName
      -> State (Set BusName) [Message]
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
fakeName = UniqueName . (":fake." ++) . unOtherName

-- vim: sw=2 sts=2
