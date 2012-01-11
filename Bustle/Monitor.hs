{-# LANGUAGE ForeignFunctionInterface #-}
module Bustle.Monitor
  (
    Monitor
  , BusType(..)
  , DebugOutput(..)

  , monitorNew
  , monitorStop
  )
where

import System.FilePath
import Control.Monad (liftM)

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C

import System.Glib.GObject
import System.Glib.GError

-- Gtk2HS boilerplate
newtype Monitor = Monitor { unMonitor :: ForeignPtr Monitor }
    deriving (Eq, Ord)

mkMonitor = (Monitor, objectUnref)

instance GObjectClass Monitor where
    toGObject = GObject . castForeignPtr . unMonitor
    unsafeCastGObject = Monitor . castForeignPtr . unGObject

-- Dirty ugly foreign imports
foreign import ccall unsafe "bustle_pcap_get_type"
    bustle_pcap_get_type :: CULong

foreign import ccall "bustle_pcap_new"
    bustle_pcap_new :: CInt
                    -> CString
                    -> CInt
                    -> Ptr (Ptr ())
                    -> IO (Ptr Monitor)
foreign import ccall "bustle_pcap_stop"
    bustle_pcap_stop :: Ptr Monitor
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

monitorNew :: BusType
           -> FilePath
           -> DebugOutput
           -> IO (Either GError Monitor)
monitorNew busType filename debugOutput =
    checkGError
        (\gerrorPtr ->
            liftM Right $
            wrapNewGObject mkMonitor $
            withCString filename $ \c_filename ->
            bustle_pcap_new (fromIntegral $ fromEnum busType)
                            c_filename
                            (fromIntegral $ fromEnum debugOutput)
                            gerrorPtr)
        (return . Left)

monitorStop :: Monitor
            -> IO ()
monitorStop monitor = do
    withForeignPtr (unMonitor monitor) bustle_pcap_stop
