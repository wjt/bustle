module Bustle.Loader
  ( readLog
  , LoadError(..)

  -- * This function bothers me, but it's used by the live recorder for now...
  , isRelevant
  )
where

import Control.Monad.Error
import Control.Arrow ((***))

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
    liftM (id *** filter (isRelevant . dmMessage)) $ case pcapResult of
        Right ms -> return ms
        Left _ -> liftM ((,) []) readOldLogFile
  where
    readOldLogFile = do
        input <- handleIOExceptions (LoadError f . show) $ readFile f
        let oldResult = fmap upgrade $ Old.readLog input
        toErrorT (\e -> LoadError f ("Parse error " ++ show e)) oldResult

isRelevant :: Message
           -> Bool
isRelevant m = case m of
    Signal {}       -> none [ senderIsBus
                            , isDisconnected
                            ]
    MethodCall {}   -> none3
    MethodReturn {} -> none3
    Error {}        -> none3
    _               -> True
  where
    -- FIXME: really? Maybe we should allow people to be interested in,
    --        say, binding to signals?
    senderIsBus = sender m == O (OtherName "org.freedesktop.DBus")
    destIsBus = destination m == O (OtherName "org.freedesktop.DBus")

    -- When the monitor is forcibly disconnected from the bus, the
    -- Disconnected message has no sender, so the logger spits out <none>.
    -- This gets turned into OtherName ""
    isDisconnected = sender m == O (OtherName "")

    none bs = not $ or bs
    none3 = none [senderIsBus, destIsBus, isDisconnected]


