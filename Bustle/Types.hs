{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
Bustle.Types: defines types used by Bustle
Copyright (C) 2008 Collabora Ltd.

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
module Bustle.Types
  ( ObjectPath
  , objectPathText

  , InterfaceName
  , interfaceNameText

  , MemberName
  , memberNameText

  , Serial

  , UniqueName(..)
  , OtherName(..)
  , TaggedBusName(..)
  , isUnique
  , isOther
  , unBusName

  , Microseconds(..)
  , µsFromPair
  , µsToMs

  , Member(..)
  , Message(..)
  , DetailedMessage(..)
  , Change(..)
  , isNameOwnerChanged
  , Log
  )
where

import Data.Word (Word32)
import DBus.Types (ObjectPath, objectPathText, InterfaceName, interfaceNameText, MemberName, memberNameText)
import DBus.Message (ReceivedMessage)
import qualified Data.Text as T

type Serial = Word32

newtype UniqueName = UniqueName { unUniqueName :: String }
  deriving (Ord, Show, Eq)
newtype OtherName = OtherName { unOtherName :: String }
  deriving (Ord, Show, Eq)
data TaggedBusName =
    U UniqueName
  | O OtherName
  deriving (Ord, Show, Eq)

isUnique, isOther :: TaggedBusName -> Bool
isUnique (U _) = True
isUnique (O _) = False
isOther = not . isUnique

unBusName :: TaggedBusName -> String
unBusName (U (UniqueName x)) = x
unBusName (O (OtherName  x)) = x

newtype Microseconds = Microseconds Integer
  deriving (Show, Ord, Eq, Num, Real, Enum, Integral)

µsFromPair :: Integer
           -> Integer
           -> Microseconds
µsFromPair s µs = Microseconds $ (s * 1000000) + µs

µsToMs :: Microseconds
       -> Integer
µsToMs (Microseconds µs) = µs `div` 1000

data Member = Member { path :: ObjectPath
                     , iface :: Maybe InterfaceName
                     , membername :: MemberName
                     }
  deriving (Ord, Show, Eq)

data Message = MethodCall { serial :: Serial
                          , sender :: TaggedBusName
                          , destination :: TaggedBusName
                          , member :: Member
                          }
             | MethodReturn { inReplyTo :: Maybe DetailedMessage
                            , sender :: TaggedBusName
                            , destination :: TaggedBusName
                            }
             | Signal { sender :: TaggedBusName
                      , signalDestination :: Maybe TaggedBusName
                      , member :: Member
                      }
             | Error { inReplyTo :: Maybe DetailedMessage
                     , sender :: TaggedBusName
                     , destination :: TaggedBusName
                     }
             | Connected { actor :: UniqueName
                         }
             | Disconnected { actor :: UniqueName
                            }
             | NameChanged { changedName :: OtherName
                           , change :: Change
                           }
  deriving (Show, Eq, Ord)

type MessageSize = Int

data DetailedMessage =
    DetailedMessage { dmTimestamp :: Microseconds
                    , dmMessage :: Message
                    , dmDetails :: Maybe (MessageSize, ReceivedMessage)
                    }
  deriving (Show, Eq)

instance Ord DetailedMessage where
    compare (DetailedMessage µs x _) (DetailedMessage µs' y _)
        = compare (µs, x) (µs', y)

data Change = Claimed UniqueName
            | Stolen UniqueName UniqueName
            | Released UniqueName
  deriving (Show, Eq, Ord)

isNameOwnerChanged :: Message -> Bool
isNameOwnerChanged (Connected {}) = True
isNameOwnerChanged (Disconnected {}) = True
isNameOwnerChanged (NameChanged {}) = True
isNameOwnerChanged _ = False

type Log = [DetailedMessage]

-- vim: sw=2 sts=2
