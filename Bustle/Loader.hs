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

import Control.Exception
import Control.Monad.Error
import Control.Arrow ((***))

import qualified Bustle.Loader.OldSkool as Old
import qualified Bustle.Loader.Pcap as Pcap
import Bustle.Upgrade (upgrade)
import Bustle.Types
import Bustle.Util (io)

data LoadError = LoadError FilePath String
instance Error LoadError where
    strMsg = LoadError ""

-- this nested case stuff is ugly, but it's less ugly than it looked with
-- combinators to turn IO (Either a b) into ErrorT LoadError IO b using various
-- a -> LoadError functions.
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
        result <- liftIO $ try $ readFile f
        case result of
            Left e      -> throwError $ LoadError f (show (e :: IOException))
            Right input -> do
                let oldResult = fmap upgrade $ Old.readLog input
                case oldResult of
                    Left e  -> throwError $ LoadError f ("Parse error " ++ show e)
                    Right r -> return r

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
    senderIsBus = sender m == busDriver
    destIsBus = destination m == busDriver
    busDriver = O (OtherName dbusName)

    -- When the monitor is forcibly disconnected from the bus, the
    -- Disconnected message has no sender; the old logger spat out <none>.
    isDisconnected = sender m == O (OtherName Old.senderWhenDisconnected)

    none bs = not $ or bs
    none3 = none [senderIsBus, destIsBus, isDisconnected]


