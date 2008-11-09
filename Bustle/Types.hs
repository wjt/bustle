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
module Bustle.Types where

import Data.Word (Word32)

type ObjectPath = String
type Interface = String
type Member = String
type ErrorName = String
type Serial = Word32
type BusName = String

type Milliseconds = Integer

data Message = MethodCall { timestamp :: Milliseconds
                          , path :: ObjectPath
                          , iface :: Interface
                          , member :: Member
                          , serial :: Serial
                          , sender :: BusName
                          , destination :: BusName
                          }
             | MethodReturn { timestamp :: Milliseconds
                            , inReplyTo :: Serial
                            , sender :: BusName
                            , destination :: BusName
                            }
             | Signal { timestamp :: Milliseconds
                      , path :: ObjectPath
                      , iface :: Interface
                      , member :: Member
                      , sender :: BusName
                      }
             | Error { timestamp :: Milliseconds
                     , name :: String
                     , inReplyTo :: Serial
                     , sender :: BusName
                     , destination :: BusName
                     }
  deriving Show

type Log = [Message]

-- vim: sw=2 sts=2
