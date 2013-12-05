{-
Bustle.Monitor: Haskell binding for pcap-monitor.c
Copyright © 2012 Collabora Ltd.

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
{-# LANGUAGE ForeignFunctionInterface #-}
module Bustle.Monitor
  (
-- * Types
    Monitor
  , BusType(..)

-- * Methods
  , monitorNew
  , monitorStop

-- * Signals
  , monitorMessageLogged
  )
where

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

-- Throws a GError if the file can't be opened, we can't get on the bus, or whatever.
monitorNew :: BusType
           -> FilePath
           -> IO Monitor
monitorNew busType filename =
    wrapNewGObject mkMonitor $
      propagateGError $ \gerrorPtr ->
        withCString filename $ \c_filename ->
          bustle_pcap_monitor_new (fromIntegral $ fromEnum busType)
                                  c_filename
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
