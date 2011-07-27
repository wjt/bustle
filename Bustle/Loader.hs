module Bustle.Loader
  ( readLog
  , LoadError(..)
  )
where

import Control.Monad.Error

import qualified Bustle.Loader.OldSkool as Old
import qualified Bustle.Loader.Pcap as Pcap
import Bustle.Upgrade (upgrade)
import Bustle.Types (Log)
import Bustle.Util (io, toErrorT, handleIOExceptions)

data LoadError = LoadError FilePath String
instance Error LoadError where
    strMsg = LoadError ""

readLog :: MonadIO io
        => FilePath
        -> ErrorT LoadError io Log
readLog f = do
    pcapResult <- io $ Pcap.readPcap f
    case pcapResult of
        Right ms -> return ms
        Left _ -> readOldLogFile
  where
    readOldLogFile = do
        input <- handleIOExceptions (LoadError f . show) $ readFile f
        let oldResult = fmap upgrade $ Old.readLog input
        toErrorT (\e -> LoadError f ("Parse error " ++ show e)) oldResult


