{-
Bustle.Loader.OldSkool: reads the output of bustle-dbus-monitor
Copyright © 2008–2011 Collabora Ltd.

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
module Bustle.Loader.OldSkool
  ( readLog
  )
where

import Bustle.Types
import Text.ParserCombinators.Parsec hiding (Parser)
import Data.Map (Map)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Control.Monad (ap, when, guard)
import Control.Applicative ((<$>))

infixl 4 <*
(<*) :: Monad m => m a -> m b -> m a
m <* n = do ret <- m; n; return ret

infixl 4 <*>
(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap

type Parser a = GenParser Char (Map (BusName, Serial) DetailedMessage) a

t :: Parser Char
t = char '\t'

nameChars :: Parser String
nameChars = many1 (noneOf "\t\n")
-- this should be
--   nameChars = many1 (oneOf "._-" <|> alphaNum)
-- but making it more tolerant lets us shoehorn misc into this field until the
-- log format is less shit.

parseUniqueName :: Parser UniqueName
parseUniqueName = do
    char ':'
    fmap (UniqueName . (':':)) nameChars
  <?> "unique name"

parseOtherName :: Parser OtherName
parseOtherName =
    fmap OtherName ((none >> return "") <|> nameChars)
  <?>
    "non-unique name"

parseBusName :: Parser BusName
parseBusName = (fmap U parseUniqueName) <|> (fmap O parseOtherName)

parseSerial :: Parser Serial
parseSerial = read <$> many1 digit <?> "serial"

parseTimestamp :: Parser Microseconds
parseTimestamp = do
    seconds <- i
    t
    µs <- i
    return $ µsFromPair seconds µs
  where i = read <$> many1 digit <?> "timestamp"

none :: Parser (Maybe a)
none = do
    string "<none>"
    return Nothing


entireMember :: Parser Member
entireMember = do
    let p = many1 (oneOf "/_" <|> alphaNum) <?> "path"
        i = none <|> fmap Just (many1 (oneOf "._" <|> alphaNum)) <?> "iface"
        m = many1 (oneOf "_" <|> alphaNum) <?> "membername"
    Member <$> p <* t <*> i <* t <*> m
  <?> "member"

addPendingCall :: DetailedMessage -> Parser ()
addPendingCall dm = updateState $ Map.insert (sender m, serial m) dm
  where
    m = dmMessage dm

findPendingCall :: BusName -> Serial -> Parser (Maybe DetailedMessage)
findPendingCall dest s = do
    pending <- getState
    let key = (dest, s)
        ret = Map.lookup key pending
    when (isJust ret) $ updateState (Map.delete key)
    return ret

methodCall :: Parser DetailedMessage
methodCall = do
    char 'c'
    t
    µs <- parseTimestamp
    t
    m <- MethodCall <$> parseSerial <* t
                    <*> parseBusName <* t <*> parseBusName <* t <*> entireMember
    let dm = DetailedMessage µs m Nothing
    addPendingCall dm
    return dm
  <?> "method call"

parseReturnOrError :: String
                   -> (Maybe DetailedMessage -> BusName -> BusName -> Message)
                   -> Parser DetailedMessage
parseReturnOrError prefix constructor = do
    string prefix <* t
    ts <- parseTimestamp <* t
    parseSerial <* t
    replySerial <- parseSerial <* t
    s <- parseBusName <* t
    d <- parseBusName
    call <- findPendingCall d replySerial
    -- If we can see a call, use its sender and destination as the destination
    -- and sender for the reply. This might prove unnecessary in the event of
    -- moving the name collapsing into the UI.
    let (s', d') = case call of
            Just (DetailedMessage _ m _) -> (destination m, sender m)
            Nothing                      -> (s, d)
        message = constructor call s' d'
    return $ DetailedMessage ts message Nothing
 <?> "method return or error"

methodReturn, parseError :: Parser DetailedMessage
methodReturn = parseReturnOrError "r" MethodReturn <?> "method return"
parseError = parseReturnOrError "err" Error <?> "error"

signal :: Parser DetailedMessage
signal = do
    string "sig"
    t
    µs <- parseTimestamp
    t
    -- Ignore serial
    m <- Signal <$> (parseSerial >> t >> parseBusName) <* t
                <*> entireMember
    return $ DetailedMessage µs m Nothing
  <?> "signal"

method :: Parser DetailedMessage
method = char 'm' >> (methodCall <|> methodReturn)
  <?> "method call or return"

noName :: Parser ()
noName = char '!' >> return ()
  <?> "the empty name '!'"

perhaps :: Parser a -> Parser (Maybe a)
perhaps act = (noName >> return Nothing) <|> fmap Just act

sameUnique :: UniqueName -> UniqueName -> Parser ()
sameUnique u u' = guard (u == u')
  <?> "owner to be " ++ unUniqueName u ++ ", not " ++ unUniqueName u'

atLeastOne :: OtherName -> Parser a
atLeastOne n = fail ""
  <?> unOtherName n ++ " to gain or lose an owner"

nameOwnerChanged :: Parser DetailedMessage
nameOwnerChanged = do
    string "nameownerchanged"
    t
    ts <- parseTimestamp
    t
    n <- parseBusName
    t
    m <- parseNOCDetails n
    return $ DetailedMessage ts m Nothing

parseNOCDetails :: BusName
                -> Parser Message
parseNOCDetails n =
    case n of
        U u -> do
            old <- perhaps parseUniqueName
            case old of
                Nothing -> do
                    t
                    u' <- parseUniqueName
                    sameUnique u u'
                    return $ Connected u
                Just u' -> do
                    sameUnique u u'
                    t
                    noName
                    return $ Disconnected u
        O o -> do
            old <- perhaps parseUniqueName
            t
            new <- perhaps parseUniqueName
            c <- case (old, new) of
                (Nothing, Nothing) -> atLeastOne o
                (Just  a, Nothing) -> return $ Released a
                (Nothing, Just  b) -> return $ Claimed b
                (Just  a, Just  b) -> return $ Stolen a b
            return $ NameChanged o c

event :: Parser DetailedMessage
event = method <|> signal <|> nameOwnerChanged <|> parseError

events :: Parser [DetailedMessage]
events = sepEndBy event (char '\n') <* eof

readLog :: String -> Either ParseError [DetailedMessage]
readLog filename = runParser events Map.empty "" filename

-- vim: sw=2 sts=2
