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

type Parser a = GenParser Char (Map (BusName, Serial) Message) a

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

parseTimestamp :: Parser Milliseconds
parseTimestamp = do
    seconds <- i
    t
    ms <- i
    return (seconds * 1000000 + ms)
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
                   -> (Milliseconds -> Maybe Message -> BusName -> BusName -> Message)
                   -> Parser Message
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
    let (s', d') = case call of Just call_ -> (destination call_, sender call_)
                                Nothing    -> (s, d)
    return $ constructor ts call s' d'
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

nameOwnerChanged :: Parser Message
nameOwnerChanged = do
    string "nameownerchanged"
    t
    ts <- parseTimestamp
    t
    n <- parseBusName
    t
    case n of
        U u -> do
            old <- perhaps parseUniqueName
            case old of
                Nothing -> do
                    t
                    u' <- parseUniqueName
                    sameUnique u u'
                    return $ Connected ts u
                Just u' -> do
                    sameUnique u u'
                    t
                    noName
                    return $ Disconnected ts u
        O o -> do
            old <- perhaps parseUniqueName
            t
            new <- perhaps parseUniqueName
            c <- case (old, new) of
                (Nothing, Nothing) -> atLeastOne o
                (Just  a, Nothing) -> return $ Released a
                (Nothing, Just  b) -> return $ Claimed b
                (Just  a, Just  b) -> return $ Stolen a b
            return $ NameChanged ts o c

event :: Parser Message
event = method <|> signal <|> nameOwnerChanged <|> parseError

events :: Parser [Message]
events = sepEndBy event (char '\n') <* eof

readLog :: String -> Either ParseError [Message]
readLog filename = filter isRelevant <$> runParser events Map.empty "" filename
  where -- FIXME: really? Maybe we should allow people to be interested in,
        --        say, binding to signals?
        senderIsBus m = sender m == O (OtherName "org.freedesktop.DBus")
        destIsBus m = destination m == O (OtherName "org.freedesktop.DBus")

        -- When the monitor is forcibly disconnected from the bus, the
        -- Disconnected message has no sender, so the logger spits out <none>.
        -- This gets turned into OtherName ""
        isDisconnected m = sender m == O (OtherName "")

        -- Surely this function must have a standard name?
        none_ fs x = not $ any ($ x) fs

        isRelevant m@(Signal {}) = none_ [ senderIsBus
                                         , isDisconnected
                                         ]
                                         m
        isRelevant m@(MethodCall {}) = none_ [ senderIsBus
                                             , destIsBus
                                             , isDisconnected
                                             ]
                                             m
        isRelevant _ = True


-- vim: sw=2 sts=2
