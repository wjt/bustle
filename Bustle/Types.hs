module Bustle.Types where

import Data.Word (Word32)

type ObjectPath = String
type Interface = String
type Member = String
type ErrorName = String
type Serial = Word32
type BusName = String

data Message = MethodCall { path :: ObjectPath
                          , iface :: Interface
                          , member :: Member
                          , serial :: Serial
                          , sender :: BusName
                          , destination :: BusName
                          }
             | MethodReturn { inReplyTo :: Serial
                            , sender :: BusName
                            , destination :: BusName
                            }
             | Signal { path :: ObjectPath
                      , iface :: Interface
                      , member :: Member
                      , sender :: BusName
                      }
             | Error { name :: String
                     , inReplyTo :: Serial
                     , sender :: BusName
                     , destination :: BusName
                     }
  deriving Show

type Log = [Message]

-- vim: sw=2 sts=2
