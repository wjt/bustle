{-# LANGUAGE ForeignFunctionInterface #-}
{-
Bustle.Util: miscellaneous utility functions
Copyright © 2008–2012 Collabora Ltd.

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
module Bustle.Util
  (
    io
  , warn

  , getCacheDir

  -- You probably don't actually want to use this function.
  , traceM

  , NonEmpty(..)
  , mapNonEmpty
  , nonEmptyToList
  )
where

import Control.Monad.Trans (MonadIO, liftIO)
import Debug.Trace (trace)
import System.IO (hPutStrLn, stderr)
import Foreign.C.String
import System.Directory
import System.FilePath ((</>))
import Bustle.Translation (__)

-- Escape hatch to log a value from a non-IO monadic context.
traceM :: (Show a, Monad m) => a -> m ()
traceM x = trace (show x) $ return ()

-- Log a warning which isn't worth showing to the user, but which might
-- interest someone debugging the application.
warn :: String -> IO ()
warn = hPutStrLn stderr . ((__ "Warning: ") ++)

-- Shorthand for liftIO.
io :: MonadIO m => IO a -> m a
io = liftIO

foreign import ccall "g_get_user_cache_dir"
    g_get_user_cache_dir :: IO CString

getCacheDir :: IO FilePath
getCacheDir = do
    dotCache <- peekCString =<< g_get_user_cache_dir
    let dir = dotCache </> "bustle"
    createDirectoryIfMissing True dir
    return dir

-- I don't want to depend on 'semigroups' for this.
data NonEmpty a = a :| [a]
    deriving (Show, Eq)

mapNonEmpty :: (a -> b)
            -> NonEmpty a
            -> NonEmpty b
mapNonEmpty f (x :| xs) = f x :| map f xs

nonEmptyToList :: NonEmpty a -> [a]
nonEmptyToList (x :| xs) = x:xs
