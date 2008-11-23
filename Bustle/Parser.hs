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
import Text.ParserCombinators.Parsec
import Data.Char (isSpace)
import Control.Monad (ap)
import Control.Applicative ((<$>))

infixl 4 <*
(<*) :: Monad m => m a -> m b -> m a
m <* n = do ret <- m; n; return ret

infixl 4 <*>
(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap

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

methodCall :: Parser Message
methodCall = do
    char 'c'
    t
    MethodCall <$> parseTimestamp <* t <*> parseSerial <* t
               <*> parseBusName <* t <*> parseBusName <* t <*> entireMember
  <?> "method call"

parseReturnOrError :: String
                   -> (Milliseconds -> Serial -> BusName -> BusName -> Message)
                   -> Parser Message
parseReturnOrError prefix constructor = do
    string prefix <* t
    constructor <$> parseTimestamp <* t
                --   own serial         serial of call
                <*> (parseSerial >> t >> parseSerial) <* t
                <*> parseBusName <* t <*> parseBusName
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
readLog = parse (sepEndBy event (char '\n') <* eof) ""


-- vim: sw=2 sts=2
