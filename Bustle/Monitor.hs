{-# LANGUAGE ForeignFunctionInterface #-}
module Bustle.Monitor
  (
-- * Types
    Monitor
  , BusType(..)
  , DebugOutput(..)

-- * Methods
  , monitorNew
  , monitorStop

-- * Signals
  , monitorMessageLogged
  )
where

import System.FilePath
import Control.Monad (liftM)

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C

import qualified Data.ByteString as BS

import System.Glib.GObject
import System.Glib.GError
import System.Glib.Signals

import Bustle.Types (Microseconds)

-- Gtk2HS boilerplate
newtype Monitor = Monitor { unMonitor :: ForeignPtr Monitor }
    deriving (Eq, Ord)

mkMonitor :: (ForeignPtr Monitor -> Monitor, FinalizerPtr a)
mkMonitor = (Monitor, objectUnref)

instance GObjectClass Monitor where
    toGObject = GObject . castForeignPtr . unMonitor
    unsafeCastGObject = Monitor . castForeignPtr . unGObject

-- Dirty ugly foreign imports
foreign import ccall "bustle_pcap_monitor_new"
    bustle_pcap_monitor_new :: CInt
                    -> CString
                    -> CInt
                    -> Ptr (Ptr ())
                    -> IO (Ptr Monitor)
foreign import ccall "bustle_pcap_monitor_stop"
    bustle_pcap_monitor_stop :: Ptr Monitor
                     -> IO ()

-- Bindings for said imports
data BusType = BusTypeNone
             | BusTypeSystem
             | BusTypeSession
  deriving
    Enum

data DebugOutput = NoDebugOutput
                 | DebugOutput
  deriving
    Enum

-- Throws a GError if the file can't be opened, we can't get on the bus, or whatever.
monitorNew :: BusType
           -> FilePath
           -> DebugOutput
           -> IO Monitor
monitorNew busType filename debugOutput =
    wrapNewGObject mkMonitor $
      propagateGError $ \gerrorPtr ->
        withCString filename $ \c_filename ->
          bustle_pcap_monitor_new (fromIntegral $ fromEnum busType)
                                  c_filename
                                  (fromIntegral $ fromEnum debugOutput)
                                  gerrorPtr

monitorStop :: Monitor
            -> IO ()
monitorStop monitor = do
    withForeignPtr (unMonitor monitor) bustle_pcap_monitor_stop

messageLoggedHandler :: (Microseconds -> BS.ByteString -> IO ())
                     -> a
                     -> Ptr ()
                     -> CInt
                     -> CLong
                     -> CLong
                     -> Ptr CChar
                     -> CUInt
                     -> IO ()
messageLoggedHandler user _obj _messageObject _isIncoming sec usec blob blobLength = do
    blobBS <- BS.packCStringLen (blob, fromIntegral blobLength)
    let µsec = fromIntegral sec * (10 ^ (6 :: Int)) + fromIntegral usec
    failOnGError $ user µsec blobBS

monitorMessageLogged :: Signal Monitor (Microseconds -> BS.ByteString -> IO ())
monitorMessageLogged =
    Signal $ \after_ obj user ->
        connectGeneric "message-logged" after_ obj $ messageLoggedHandler user
