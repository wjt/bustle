{-# LANGUAGE PatternGuards #-}
module Bustle.Pcap
  ( readPcap
  )
where

import Data.Either (partitionEithers, either)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Applicative ((<$>))
import Control.Exception (try)

import Network.Pcap

import DBus.Constants (dbusName, dbusInterface)
import DBus.Wire
import DBus.Message
import DBus.Types
import qualified Data.Text.Lazy as T

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromChunks)
import qualified Data.Binary.Get as G

import qualified Bustle.Types as B

-- Conversions from dbus-core's types into Bustle's more stupid types. This
-- whole section is pretty upsetting.
convertBusName :: String
               -> Maybe BusName
               -> B.BusName
convertBusName context n =
    case rawName of
        (':':_) -> B.U $ B.UniqueName rawName
        _       -> B.O $ B.OtherName rawName
  where
    rawName = maybe context (T.unpack . strBusName) n

convertMember :: (a -> ObjectPath)
              -> (a -> Maybe InterfaceName)
              -> (a -> MemberName)
              -> a
              -> B.Member
convertMember getObjectPath getInterfaceName getMemberName m =
    B.Member (T.unpack . strObjectPath . getObjectPath $ m)
             (fmap (T.unpack . strInterfaceName) . getInterfaceName $ m)
             (T.unpack . strMemberName . getMemberName $ m)

type PendingMessages = Map (B.BusName, B.Serial) B.Message

withInReplyTo :: PendingMessages
              -> B.BusName
              -> Serial
              -> (Maybe B.Message -> a)
              -> (PendingMessages, a)
withInReplyTo pending name serial f =
    (Map.delete key pending, f irt)
  where
    key = (name, serialValue serial)
    irt = Map.lookup key pending

insertPending :: PendingMessages -> B.Message -> (PendingMessages, B.Message)
insertPending pending b = (pending', b)
  where
    pending' = Map.insert (B.sender b, B.serial b) b pending

isNOC :: Maybe BusName -> Signal -> Maybe (BusName, Maybe BusName, Maybe BusName)
isNOC (Just sender) s | looksLikeNOC =
    case names of
        [Just n, old, new] -> Just (n, old, new)
        _                  -> Nothing
  where
    names :: [Maybe BusName]
    names = map fromVariant $ signalBody s

    looksLikeNOC =
        and [ sender == dbusName
            , signalInterface s == dbusInterface
            , strMemberName (signalMember s) == T.pack "NameOwnerChanged"
            ]
isNOC _ _ = Nothing


bustlifyNOC :: B.Milliseconds -> (BusName, Maybe BusName, Maybe BusName) -> B.Message
bustlifyNOC ms ns@(name, oldOwner, newOwner)
    | isUnique name =
          case (oldOwner, newOwner) of
              (Just _, Nothing) -> B.Connected ms (uniquify name)
              (Nothing, Just _) -> B.Disconnected ms (uniquify name)
              _                 -> error $ "wtf: NOC" ++ show ns
    | otherwise = B.NameChanged ms (otherify name) $
          case (oldOwner, newOwner) of
              (Just old, Nothing)  -> B.Released (uniquify old)
              (Just old, Just new) -> B.Stolen (uniquify old) (uniquify new)
              (Nothing, Just new)  -> B.Claimed (uniquify new)
              (Nothing, Nothing)   -> error $ "wtf: NOC" ++ show ns
  where
    isUnique :: BusName -> Bool
    isUnique n = T.head (strBusName n) == ':'

    uniquify = B.UniqueName . T.unpack . strBusName
    otherify = B.OtherName . T.unpack . strBusName

bustlify :: PendingMessages
         -> B.Milliseconds
         -> ReceivedMessage
         -> (PendingMessages, B.Message)
bustlify pending ms m =
    case m of
        (ReceivedMethodCall serial sender mc) ->
            insertPending pending $
                B.MethodCall { B.timestamp = ms
                             , B.serial = serialValue serial
                             -- sender may be empty if it's us who sent it
                             , B.sender = convertBusName "method.call.sender" sender
                             , B.destination = convertBusName "method.call.destination" $ methodCallDestination mc
                             , B.member = convertMember methodCallPath methodCallInterface methodCallMember mc
                             }
        (ReceivedMethodReturn _serial sender mr) ->
            withInReplyTo pending (convertBusName "method.return.destination" $ methodReturnDestination mr) (methodReturnSerial mr) $ \irt ->
                B.MethodReturn { B.timestamp = ms
                               , B.inReplyTo = irt
                               , B.sender = convertBusName "method.return.sender" sender
                               , B.destination = convertBusName "method.return.destination" $ methodReturnDestination mr
                               }
        (ReceivedError _serial sender e) ->
            withInReplyTo pending (convertBusName "method.error.destination" $ errorDestination e) (errorSerial e) $ \irt ->
                B.Error { B.timestamp = ms
                        , B.inReplyTo = irt
                        , B.sender = convertBusName "method.error.sender" sender
                        , B.destination = convertBusName "method.error.destination" $ errorDestination e
                        }
        (ReceivedSignal _serial sender sig)
            | Just names <- isNOC sender sig -> (pending, bustlifyNOC ms names)
            | otherwise                      -> (pending,
                B.Signal { B.timestamp = ms
                         , B.sender = convertBusName "signal.sender" sender
                         , B.member = convertMember signalPath (Just . signalInterface) signalMember sig
                         })
        (ReceivedUnknown _ _ _) -> error "wtf"

-- This is stolen essentially verbatim from benchUnmarshal in dbus-core's
-- benchmark.hs
unmarshalFromBS :: BS.ByteString -> Either UnmarshalError ReceivedMessage
unmarshalFromBS bs = G.runGet getter $ fromChunks [bs]
  where
    getter = unmarshalMessage getBytes
    getBytes = G.getLazyByteString . fromIntegral

fromPacket :: PktHdr
           -> BS.ByteString
           -> Either UnmarshalError (B.Milliseconds, ReceivedMessage)
fromPacket hdr body =
    case unmarshalFromBS body of
        Left e  -> Left e
        Right m -> Right (ms, m)
  where
    ms = fromIntegral (hdrTime hdr) `div` 1000

convert :: PktHdr
        -> BS.ByteString
        -> PendingMessages
        -> Either String (PendingMessages, B.Message)
convert hdr body s =
    either (Left . show) (Right . uncurry (bustlify s)) $ fromPacket hdr body

mapBodies :: PcapHandle
          -> (PktHdr -> BS.ByteString -> s -> Either e (s, a))
          -> s
          -> IO (Either e [a])
mapBodies p f s = do
    (hdr, body) <- nextBS p
    -- No really, nextBS just returns null packets when you hit the end of the
    -- file.
    --
    -- It occurs to me that we could stream by just polling this every second
    -- or something?
    if hdrCaptureLength hdr == 0
        then return $ Right []
        else do
            let x = f hdr body s
            case x of
                Left e -> return $ Left e
                Right (s', a) -> do
                    xs <- mapBodies p f s'
                    case xs of
                        Left e -> return $ Left e
                        Right as -> return $ Right (a:as)

readPcap :: FilePath -> IO (Either IOError [B.Message])
readPcap path = try $ do
    p <- openOffline path

    ret <- mapBodies p convert Map.empty
    -- FIXME: make the error handling less shoddy
    case ret of
        Left e -> error $ show e
        Right xs -> return xs
