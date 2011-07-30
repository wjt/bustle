{-# LANGUAGE PatternGuards #-}
module Bustle.Loader.Pcap
  ( readPcap
  )
where

import Data.Either (partitionEithers, either)
import Data.Maybe (fromMaybe, catMaybes, fromJust)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Applicative ((<$>))
import Control.Exception (try)
import Control.Monad.State

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

type PendingMessages = Map (Maybe BusName, Serial) (MethodCall, B.DetailedMessage)

popMatchingCall :: Maybe BusName
                -> Serial
                -> State PendingMessages (Maybe (MethodCall, B.DetailedMessage))
popMatchingCall name serial = do
    ret <- tryPop (name, serial)
    case (ret, name) of
        -- If we don't get an answer, but we know a destination, this may be
        -- because we didn't know the sender's bus name because it was the
        -- logger itself. So try looking up pending replies whose sender is
        -- Nothing.
        (Nothing, Just _) -> tryPop (Nothing, serial)
        _                 -> return ret
  where
    tryPop key = do
        call <- gets $ Map.lookup key
        modify $ Map.delete key
        return call

insertPending :: Maybe BusName -> Serial -> MethodCall -> B.DetailedMessage -> State PendingMessages ()
insertPending n s rawCall b = modify $ Map.insert (n, s) (rawCall, b)

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


bustlifyNOC :: (BusName, Maybe BusName, Maybe BusName)
            -> B.Message
bustlifyNOC ns@(name, oldOwner, newOwner)
    | isUnique name =
          case (oldOwner, newOwner) of
              (Just _, Nothing) -> B.Connected (uniquify name)
              (Nothing, Just _) -> B.Disconnected (uniquify name)
              _                 -> error $ "wtf: NOC" ++ show ns
    | otherwise = B.NameChanged (otherify name) $
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

bustlify :: B.Microseconds
         -> ReceivedMessage
         -> State PendingMessages B.DetailedMessage
bustlify µs m = do
    bm <- buildBustledMessage
    return $ B.DetailedMessage µs bm (Just m)
  where
    buildBustledMessage = case m of
        (ReceivedMethodCall serial sender mc) -> do
            let call = B.MethodCall
                             { B.serial = serialValue serial
                             -- sender may be empty if it's us who sent it
                             , B.sender = convertBusName "method.call.sender" sender
                             , B.destination = convertBusName "method.call.destination" $ methodCallDestination mc
                             , B.member = convertMember methodCallPath methodCallInterface methodCallMember mc
                             }
            -- FIXME: we shouldn't need to construct the same DetailedMessage
            -- both here and 10 lines above.
            insertPending sender serial mc (B.DetailedMessage µs call (Just m))
            return call

        (ReceivedMethodReturn _serial sender mr) -> do
            call <- popMatchingCall (methodReturnDestination mr) (methodReturnSerial mr)

            return $ case call of
                Just (rawCall, dm)
                    -- FIXME: obviously this should be more robust:
                    --  • check that the service really is the bus daemon
                    --  • don't crash if the body of the call or reply doesn't contain one bus name.
                    | B.membername (B.member (B.dmMessage dm)) == "GetNameOwner"
                        -> bustlifyNOC ( fromJust . fromVariant $ (methodCallBody rawCall !! 0)
                                       , Nothing
                                       , fromVariant $ (methodReturnBody mr !! 0)
                                       )
                _ -> B.MethodReturn
                               { B.inReplyTo = fmap snd call
                               , B.sender = convertBusName "method.return.sender" sender
                               , B.destination = convertBusName "method.return.destination" $ methodReturnDestination mr
                               }

        (ReceivedError _serial sender e) -> do
            call <- popMatchingCall (errorDestination e) (errorSerial e)
            return $ B.Error
                        { B.inReplyTo = fmap snd call
                        , B.sender = convertBusName "method.error.sender" sender
                        , B.destination = convertBusName "method.error.destination" $ errorDestination e
                        }

        (ReceivedSignal _serial sender sig)
            | Just names <- isNOC sender sig -> return $ bustlifyNOC names
            | otherwise                      -> return $
                B.Signal { B.sender = convertBusName "signal.sender" sender
                         , B.member = convertMember signalPath (Just . signalInterface) signalMember sig
                         }

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
           -> Either UnmarshalError (B.Microseconds, ReceivedMessage)
fromPacket hdr body =
    case unmarshalFromBS body of
        Left e  -> Left e
        Right m -> Right (µs, m)
  where
    µs = fromIntegral (hdrTime hdr)

convert :: PktHdr
        -> BS.ByteString
        -> PendingMessages
        -> Either String (B.DetailedMessage, PendingMessages)
convert hdr body s =
    case fromPacket hdr body of
        Left unmarshalError -> Left $ show unmarshalError
        Right (ms, m)       -> Right $ runState (bustlify ms m) s

mapBodies :: PcapHandle
          -> (PktHdr -> BS.ByteString -> s -> Either e (a, s))
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
                Right (a, s') -> do
                    xs <- mapBodies p f s'
                    case xs of
                        Left e -> return $ Left e
                        Right as -> return $ Right (a:as)

readPcap :: FilePath -> IO (Either IOError [B.DetailedMessage])
readPcap path = try $ do
    p <- openOffline path

    ret <- mapBodies p convert Map.empty
    -- FIXME: make the error handling less shoddy
    case ret of
        Left e -> error $ show e
        Right xs -> return xs
