{-
Bustle.Parser: reads the output of dbus-monitor --profile
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
module Bustle.Parser (readLog)
where

import Bustle.Types
import Text.ParserCombinators.Parsec hiding (Parser)
import Data.Char (isSpace)
import Data.Map (Map)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Control.Monad (ap, when)
import Control.Applicative ((<$>))

infixl 4 <*
(<*) :: Monad m => m a -> m b -> m a
m <* n = do ret <- m; n; return ret

infixl 4 <*>
(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap

type Parser a = GenParser Char (Map (BusName, Serial) Message) a

t :: Parser Char
t = char '\t'

parseBusName :: Parser BusName
parseBusName = many1 (oneOf ":._-" <|> alphaNum) <?> "bus name"

parseSerial :: Parser Serial
parseSerial = read <$> many1 digit <?> "serial"

parseTimestamp :: Parser Milliseconds
parseTimestamp = do
    seconds <- i
    t
    ms <- i
    return (seconds * 1000000 + ms)
  where i = read <$> many1 digit <?> "timestamp"

entireMember :: Parser Member
entireMember = do
    let p = many1 (oneOf "/_" <|> alphaNum) <?> "path"
        i = many1 (oneOf "._" <|> alphaNum) <?> "iface"
        m = many1 (oneOf "_" <|> alphaNum) <?> "membername"
    Member <$> p <* t <*> i <* t <*> m
  <?> "member"

addPendingCall :: Message -> Parser ()
addPendingCall m = updateState $ Map.insert (sender m, serial m) m

findPendingCall :: BusName -> Serial -> Parser (Maybe Message)
findPendingCall dest s = do
    pending <- getState
    let key = (dest, s)
        ret = Map.lookup key pending
    when (isJust ret) $ updateState (Map.delete key)
    return ret

methodCall :: Parser Message
methodCall = do
    char 'c'
    t
    m <- MethodCall <$> parseTimestamp <* t <*> parseSerial <* t
                    <*> parseBusName <* t <*> parseBusName <* t <*> entireMember
    addPendingCall m
    return m
  <?> "method call"

parseReturnOrError :: String
                   -> (Milliseconds -> Message -> BusName -> BusName -> Message)
                   -> Parser Message
parseReturnOrError prefix constructor = do
    string prefix <* t
    ts <- parseTimestamp <* t
    parseSerial <* t
    replySerial <- parseSerial <* t
    s <- parseBusName <* t
    d <- parseBusName
    call <- findPendingCall d replySerial
    case call of
      Just m  -> return $ constructor ts m s d
      Nothing -> fail "return doesn't match any calls"
 <?> "method return or error"

methodReturn, parseError :: Parser Message
methodReturn = parseReturnOrError "r" MethodReturn <?> "method return"
parseError = parseReturnOrError "err" Error <?> "error"

signal :: Parser Message
signal = do
    string "sig"
    t
    -- Ignore serial
    Signal <$> parseTimestamp <* t <*> (parseSerial >> t >> parseBusName) <* t
           <*> entireMember
  <?> "signal"

method :: Parser Message
method = char 'm' >> (methodCall <|> methodReturn)
  <?> "method call or return"

event :: Parser Message
event = method <|> signal <|> parseError

readLog :: String -> Either ParseError [Message]
readLog = runParser (sepEndBy event (char '\n') <* eof) Map.empty ""


-- vim: sw=2 sts=2
