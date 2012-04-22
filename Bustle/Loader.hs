{-
Bustle.Loader: loads logs using one of the two sub-loaders
Copyright © 2011–2012 Collabora Ltd.

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
module Bustle.Loader
  ( readLog
  , LoadError(..)

  -- * This function bothers me, but it's used by the live recorder for now...
  , isRelevant
  )
where

import Control.Monad.Error
import Control.Arrow ((***))

import qualified Data.Text as Text
import DBus.Constants (dbusName)
import DBus.Types (busNameText)

import qualified Bustle.Loader.OldSkool as Old
import qualified Bustle.Loader.Pcap as Pcap
import Bustle.Upgrade (upgrade)
import Bustle.Types
import Bustle.Util (io, toErrorT, handleIOExceptions)

data LoadError = LoadError FilePath String
instance Error LoadError where
    strMsg = LoadError ""

readLog :: MonadIO io
        => FilePath
        -> ErrorT LoadError io ([String], Log)
readLog f = do
    pcapResult <- io $ Pcap.readPcap f
    liftM (id *** filter (isRelevant . deEvent)) $ case pcapResult of
        Right ms -> return ms
        Left _ -> liftM ((,) []) readOldLogFile
  where
    readOldLogFile = do
        input <- handleIOExceptions (LoadError f . show) $ readFile f
        let oldResult = fmap upgrade $ Old.readLog input
        toErrorT (\e -> LoadError f ("Parse error " ++ show e)) oldResult

isRelevant :: Event
           -> Bool
isRelevant (NOCEvent _) = True
isRelevant (MessageEvent m) = case m of
    Signal {}       -> none [ senderIsBus
                            , isDisconnected
                            ]
    MethodCall {}   -> none3
    MethodReturn {} -> none3
    Error {}        -> none3
  where
    -- FIXME: really? Maybe we should allow people to be interested in,
    --        say, binding to signals?
    senderIsBus = sender m == O (OtherName (busNameText dbusName))
    destIsBus = destination m == O (OtherName (busNameText dbusName))

    -- When the monitor is forcibly disconnected from the bus, the
    -- Disconnected message has no sender, so the logger spits out <none>.
    -- This gets turned into OtherName ""
    isDisconnected = sender m == O (OtherName Text.empty)

    none bs = not $ or bs
    none3 = none [senderIsBus, destIsBus, isDisconnected]


