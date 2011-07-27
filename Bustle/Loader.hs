module Bustle.Loader
  ( readLog
  , LoadError(..)
  )
where

import Control.Monad.Error

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
        -> ErrorT LoadError io Log
readLog f = do
    pcapResult <- io $ Pcap.readPcap f
    liftM (filter (isRelevant . dmMessage)) $ case pcapResult of
        Right ms -> return ms
        Left _ -> readOldLogFile
  where
    readOldLogFile = do
        input <- handleIOExceptions (LoadError f . show) $ readFile f
        let oldResult = fmap upgrade $ Old.readLog input
        toErrorT (\e -> LoadError f ("Parse error " ++ show e)) oldResult

    -- FIXME: really? Maybe we should allow people to be interested in,
    --        say, binding to signals?
    senderIsBus m = sender m == O (OtherName "org.freedesktop.DBus")
    destIsBus m = destination m == O (OtherName "org.freedesktop.DBus")

    -- When the monitor is forcibly disconnected from the bus, the
    -- Disconnected message has no sender, so the logger spits out <none>.
    -- This gets turned into OtherName ""
    isDisconnected m = sender m == O (OtherName "")

    -- Surely this function must have a standard name?
    none_ fs x = not $ any ($ x) fs

    isRelevant m@(Signal {}) = none_ [ senderIsBus
                                     , isDisconnected
                                     ]
                                     m
    isRelevant m@(MethodCall {}) = none_ [ senderIsBus
                                         , destIsBus
                                         , isDisconnected
                                         ]
                                         m
    isRelevant _ = True


