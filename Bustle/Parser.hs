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
