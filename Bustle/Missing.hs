{-
Bustle.Missing: missing GLib bindings
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
module Bustle.Missing
  ( formatSize
  )
where

import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import System.Glib.UTFString

foreign import ccall "g_format_size"
    g_format_size :: CULLong
                  -> IO CString

formatSize :: Int -- (could be Word64 but D-Bus' max message size is 2 ** 27)
           -> String
formatSize size = unsafePerformIO $ do
    ret <- g_format_size (fromIntegral size)
    readUTFString ret
