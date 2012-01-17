{-# LANGUAGE PatternGuards, FlexibleContexts #-}
module Bustle.Loader.Pcap
  ( readPcap

  , convert
  )
where

import Data.Maybe (fromJust, fromMaybe)
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Exception (try)
import Control.Monad.State

import Network.Pcap

import DBus.Constants (dbusName, dbusInterface)
import DBus.Wire
import DBus.Message
import DBus.Types
import qualified Data.Text as T

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromChunks)
import qualified Data.Binary.Get as G

import qualified Bustle.Types as B

-- Conversions from dbus-core's types into Bustle's more stupid types. This
-- whole section is pretty upsetting.
stringifyBusName :: BusName
                 -> String
stringifyBusName = T.unpack . busNameText

stupifyBusName :: String
               -> B.BusName
stupifyBusName n =
    case n of
        (':':_) -> B.U $ B.UniqueName n
        _       -> B.O $ B.OtherName n

convertBusName :: String
               -> Maybe BusName
               -> B.BusName
convertBusName context n = stupifyBusName (maybe context stringifyBusName n)

convertMember :: (a -> ObjectPath)
              -> (a -> Maybe InterfaceName)
              -> (a -> MemberName)
              -> a
              -> B.Member
convertMember getObjectPath getInterfaceName getMemberName m =
    B.Member (T.unpack . objectPathText . getObjectPath $ m)
             (fmap (T.unpack . interfaceNameText) . getInterfaceName $ m)
             (T.unpack . memberNameText . getMemberName $ m)

type PendingMessages = Map (Maybe BusName, Serial)
                           (MethodCall, B.DetailedMessage)

popMatchingCall :: (MonadState PendingMessages m)
                => Maybe BusName
                -> Serial
                -> m (Maybe (MethodCall, B.DetailedMessage))
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

insertPending :: (MonadState PendingMessages m)
              => Maybe BusName
              -> Serial
              -> MethodCall
              -> B.DetailedMessage
              -> m ()
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
            , memberNameText (signalMember s) == T.pack "NameOwnerChanged"
            ]
isNOC _ _ = Nothing


bustlifyNOC :: (BusName, Maybe BusName, Maybe BusName)
            -> B.Message
bustlifyNOC ns@(name, oldOwner, newOwner)
    | isUnique name =
          case (oldOwner, newOwner) of
              (Nothing, Just _) -> B.Connected (uniquify name)
              (Just _, Nothing) -> B.Disconnected (uniquify name)
              _                 -> error $ "wtf: NOC" ++ show ns
    | otherwise = B.NameChanged (otherify name) $
          case (oldOwner, newOwner) of
              (Just old, Nothing)  -> B.Released (uniquify old)
              (Just old, Just new) -> B.Stolen (uniquify old) (uniquify new)
              (Nothing, Just new)  -> B.Claimed (uniquify new)
              (Nothing, Nothing)   -> error $ "wtf: NOC" ++ show ns
  where
    isUnique :: BusName -> Bool
    isUnique n = T.head (busNameText n) == ':'

    uniquify = B.UniqueName . T.unpack . busNameText
    otherify = B.OtherName . T.unpack . busNameText

bustlify :: Monad m
         => B.Microseconds
         -> Int
         -> ReceivedMessage
         -> StateT PendingMessages m B.DetailedMessage
bustlify µs bytes m = do
    bm <- buildBustledMessage
    return $ B.DetailedMessage µs bm (Just (bytes, m))
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
            insertPending sender serial mc (B.DetailedMessage µs call (Just (bytes, m)))
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
                         , B.signalDestination = fmap (stupifyBusName . stringifyBusName)
                                               $ signalDestination sig
                         }

        (ReceivedUnknown _ _ _) -> error "wtf"

convert :: Monad m
        => B.Microseconds
        -> BS.ByteString
        -> StateT PendingMessages m (Either String B.DetailedMessage)
convert µs body =
    case unmarshalMessage body of
        Left unmarshalError -> return $ Left $ show unmarshalError
        Right m             -> liftM Right $ bustlify µs (BS.length body) m

data Result e a =
    EOF
  | Packet (Either e a)
  deriving Show

readOne :: (Monad m, MonadIO m)
        => PcapHandle
        -> (B.Microseconds -> BS.ByteString -> StateT s m (Either e a))
        -> StateT s m (Result e a)
readOne p f = do
    (hdr, body) <- liftIO $ nextBS p
    -- No really, nextBS just returns null packets when you hit the end of the
    -- file.
    --
    -- It occurs to me that we could stream by just polling this every second
    -- or something?
    if hdrCaptureLength hdr == 0
        then return EOF
        else liftM Packet $ f (fromIntegral (hdrTime hdr)) body

mapBodies :: (Monad m, MonadIO m)
          => PcapHandle
          -> (B.Microseconds -> BS.ByteString -> StateT s m (Either e a))
          -> StateT s m [Either e a]
mapBodies p f = do
    ret <- readOne p f
    case ret of
        EOF      -> return $ []
        Packet x -> do
            xs <- mapBodies p f
            return $ x:xs

readPcap :: FilePath
         -> IO (Either IOError ([String], [B.DetailedMessage]))
readPcap path = try $ do
    p <- openOffline path

    liftM partitionEithers $ evalStateT (mapBodies p convert) Map.empty
