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
import Control.Applicative ((<$>))

parseBusName :: Parser BusName
parseBusName = many1 (oneOf ":._-" <|> alphaNum) <?> "bus name"

parsePath :: Parser ObjectPath
parsePath = many1 (oneOf "/_" <|> alphaNum) <?> "path"

parseIface :: Parser Interface
parseIface = many1 (oneOf "._" <|> alphaNum) <?> "iface"

parseMember :: Parser Member
parseMember = many1 (oneOf "_" <|> alphaNum) <?> "member"

parseSerial = read <$> many1 digit <?> "serial"

parseTimestamp :: Parser Int
parseTimestamp = i >> t >> i
    where i = read <$> many1 digit <?> "timestamp"

t = char '\t'

entireMember :: Parser (ObjectPath, Interface, Member)
entireMember = do
    path <- parsePath
    t
    iface <- parseIface
    t
    member <- parseMember

    return (path, iface, member)
  <?> "path-iface-member"

methodCall :: Parser Message
methodCall = do
    char 'c'
    t
    parseTimestamp
    t
    serial <- parseSerial
    t
    sender <- parseBusName
    t
    destination <- parseBusName
    t
    (path, iface, member) <- entireMember

    return (MethodCall path iface member serial sender destination)
  <?> "method call"

methodReturn :: Parser Message
methodReturn = do
    char 'r'
    t
    parseTimestamp
    t
    serial <- parseSerial
    t
    replySerial <- parseSerial
    t
    sender <- parseBusName
    t
    destination <- parseBusName

    return (MethodReturn replySerial sender destination)
  <?> "method return"

signal :: Parser Message
signal = do
    string "sig"
    t
    parseTimestamp
    t
    serial <- parseSerial
    t
    sender <- parseBusName
    t
    (path, iface, member) <- entireMember

    return (Signal path iface member sender)
  <?> "signal"


method = char 'm' >> (methodCall <|> methodReturn)

event = method <|> signal

readLog :: String -> Either ParseError [Message]
readLog = parse (sepEndBy event (char '\n')) ""


-- vim: sw=2 sts=2
