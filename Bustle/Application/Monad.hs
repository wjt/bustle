{-
Bustle.Application.Monad: Implementation of the monad used for the UI
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
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             FlexibleInstances #-}
module Bustle.Application.Monad
  (
  -- ^ The Bustle monad
    Bustle
  , runB

  -- ^ Tunnelling goo
  , BustleEnv -- but not the internals
  , embedIO
  , makeCallback
  )
where

import Control.Monad.Reader
import Control.Monad.State

import Data.IORef

{- The goal is to have the standard Reader/State stack for immutable and
 - mutable application state, but also be able to reconstitute it inside GLib
 - callbacks (which are in IO).
 -
 - We implement this by storing both the configuration and the state in an
 - IORef, and provide functions to reconstitute the environment inside a
 - callback. Inspired by this excellent email, titled Monadic Tunnelling:
 -   <http://www.haskell.org/pipermail/haskell-cafe/2007-July/028501.html>
 -
 - You're intended to write 'type B a = Bustle SomeConfig SomeState a' for
 - brevity. Then, within a 'B foo' action, if you want to connect to a GLib
 - signal, you say something like this:
 -
 -    onDance :: Badger -> IO a -> IO ()
 -    dancedCB :: B a
 -
 -    embedIO $ onDance x . makeCallback dancedCB
 -}
newtype Bustle config state a = B (ReaderT (BustleEnv config state) IO a)
  deriving (Functor, Monad, MonadIO)

newtype BustleEnv config state =
    BustleEnv { unBustleEnv :: IORef (config, state) }

readConfig :: MonadIO m
           => BustleEnv config state
           -> m config
readConfig = liftM fst . liftIO . readIORef . unBustleEnv

readState :: MonadIO m
          => BustleEnv config state
          -> m state
readState  = liftM snd . liftIO . readIORef . unBustleEnv

putState :: MonadIO m
         => state
         -> BustleEnv config state
         -> m ()
putState new e = liftIO $ do
    modifyIORef (unBustleEnv e) $ \(conf, _) -> (conf, new)

instance MonadState state (Bustle config state) where
  get = B $ ask >>= readState
  put x = B $ ask >>= putState x

instance MonadReader config (Bustle config state) where
    ask = B $ ask >>= readConfig
    -- FIXME: I don't actually think it's possible to implement local without
    -- keeping two refs or something. I guess I could make a temporary ioref,
    -- and propagate any changes to the actual state part of the ref to the
    -- outside world. This would break horribly in the face of threads. Or we
    -- could do something like:
    --   MVar (BConfig, MVar BState)
    local = error "Sorry, Dave, I can't let you do that."

embedIO :: (BustleEnv config state -> IO a) -> Bustle config state a
embedIO act = B $ do
  r <- ask
  liftIO $ act r

makeCallback :: Bustle config state a -> BustleEnv config state -> IO a
makeCallback (B act) x = runReaderT act x

runB :: config -> state -> Bustle config state a -> IO a
runB config s (B act) = do
    r <- newIORef (config, s)
    runReaderT act $ BustleEnv r
