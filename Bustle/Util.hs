{-
Bustle.Util: miscellaneous utility functions
Copyright © 2008–2010 Collabora Ltd.

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

  , toErrorT
  , handleIOExceptions

  -- You probably don't actually want to use this function.
  , traceM
  )
where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Trans (MonadIO, liftIO)
import Debug.Trace (trace)
import System.IO (hPutStrLn, stderr)

-- Escape hatch to log a value from a non-IO monadic context.
traceM :: (Show a, Monad m) => a -> m ()
traceM x = trace (show x) $ return ()

-- Log a warning which isn't worth showing to the user, but which might
-- interest someone debugging the application.
warn :: String -> IO ()
warn = hPutStrLn stderr . ("Warning: " ++)

-- Shorthand for liftIO.
io :: MonadIO m => IO a -> m a
io = liftIO

-- Converts an Either to an action in an ErrorT.
toErrorT :: (Monad m, Error e')
         => (e -> e')     -- ^ Convert a Left value to the error type e'
         -> Either e a    -- ^ A possibly-erroneous value
         -> ErrorT e' m a -- ^ A happily-tranformed value
toErrorT f = either (throwError . f) return

-- Catches IOExceptions , and maps them into ErrorT. Need a better name.

handleIOExceptions :: (Error e', MonadIO io)
                   => (IOException -> e') -- ^ Transform an IO exception to our
                                          --   error type
                   -> IO a -- ^ an action which may throw an IOException
                   -> ErrorT e' io a -- ^ woo yay
handleIOExceptions f act = do
    result <- io $ try act
    toErrorT f result
