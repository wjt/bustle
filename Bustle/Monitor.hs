{-
Bustle.Monitor: Haskell binding for pcap-monitor.c
Copyright © 2012 Collabora Ltd.
Copyright © 2018 Will Thompson

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
  , monitorStopped
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

-- Foreign imports
foreign import ccall "bustle_pcap_monitor_new"
    bustle_pcap_monitor_new :: CInt
                    -> CString
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
monitorNew :: Either BusType String
           -> FilePath
           -> IO Monitor
monitorNew target filename =
    wrapNewGObject mkMonitor $
      propagateGError $ \gerrorPtr ->
        withAddress $ \c_address ->
          withCString filename $ \c_filename ->
            bustle_pcap_monitor_new c_busType c_address c_filename gerrorPtr
  where
    c_busType = fromIntegral . fromEnum $ case target of
        Left busType  -> busType
        Right _       -> BusTypeNone
    withAddress f = case target of
        Left _        -> f nullPtr
        Right address -> withCString address f

monitorStop :: Monitor
            -> IO ()
monitorStop monitor = do
    withForeignPtr (unMonitor monitor) bustle_pcap_monitor_stop

messageLoggedHandler :: (Microseconds -> BS.ByteString -> IO ())
                     -> a
                     -> CLong
                     -> CLong
                     -> Ptr CChar
                     -> CUInt
                     -> IO ()
messageLoggedHandler user _obj sec usec blob blobLength = do
    blobBS <- BS.packCStringLen (blob, fromIntegral blobLength)
    let µsec = fromIntegral sec * (10 ^ (6 :: Int)) + fromIntegral usec
    failOnGError $ user µsec blobBS

monitorMessageLogged :: Signal Monitor (Microseconds -> BS.ByteString -> IO ())
monitorMessageLogged =
    Signal $ \after_ obj user ->
        connectGeneric "message-logged" after_ obj $ messageLoggedHandler user

stoppedHandler :: (Quark -> Int -> String -> IO ())
             -> a
             -> CUInt
             -> CInt
             -> Ptr CChar
             -> IO ()
stoppedHandler user _obj domain code messagePtr = do
    message <- peekCString messagePtr
    failOnGError $ user domain (fromIntegral code) message

monitorStopped :: Signal Monitor (Quark -> Int -> String -> IO ())
monitorStopped =
    Signal $ \after_ obj user ->
        connectGeneric "stopped" after_ obj $ stoppedHandler user
