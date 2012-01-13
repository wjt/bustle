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

import System.Glib.GObject
import System.Glib.GError
import System.Glib.Signals

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

monitorMessageLogged :: Signal Monitor (IO ())
monitorMessageLogged =
    Signal $ \after_ obj user ->
        connectGeneric "message-logged" after_ obj $ \_obj ->
            failOnGError user
