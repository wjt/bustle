{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-
Bustle.Marquee: My First Type-Safe Markup Library With A Cutesy Name To Not Collide With Pango's 'Markup' Which Is A Synonym For String
Copyright © 2011 Will Thompson

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
module Bustle.Marquee
  ( Marquee
  , toPangoMarkup
  , tag
  , b
  , i
  , small
  , light
  , red
  , a
  , escape

  , formatMember

  , toString
  )
where

import Data.Text (Text)
import qualified Data.Text as T

import Graphics.Rendering.Pango.BasicTypes (Weight(..))
import Graphics.Rendering.Pango.Layout (escapeMarkup)
import Graphics.Rendering.Pango.Markup (markSpan, SpanAttribute(..))

import Bustle.Types (ObjectPath, formatObjectPath, InterfaceName, formatInterfaceName, MemberName, formatMemberName)

newtype Marquee = Marquee { unMarquee :: String }
    deriving (Show, Read, Ord, Eq)

toPangoMarkup :: Marquee -> String
toPangoMarkup = unMarquee

instance Monoid Marquee where
    mempty = Marquee ""
    mappend x y = Marquee (unMarquee x `mappend` unMarquee y)
    mconcat = Marquee . mconcat . map unMarquee

tag :: String -> Marquee -> Marquee
tag name contents =
    Marquee $ concat [ "<", name, ">"
                    , unMarquee contents
                    , "</", name, ">"
                    ]

b, i, small :: Marquee -> Marquee
b = tag "b"
i = tag "i"
small = tag "small"

a :: String
  -> String
  -> Marquee
a href contents =
  Marquee $ concat [ "<a href=\"", escapeMarkup href, "\">"
                  , escapeMarkup contents
                  , "</a>"
                  ]

span_ :: [SpanAttribute] -> Marquee -> Marquee
span_ attrs = Marquee . markSpan attrs . unMarquee

light :: Marquee -> Marquee
light = span_ [FontWeight WeightLight]

red :: Marquee -> Marquee
red = span_ [FontForeground "#ff0000"]

-- Kind of a transitional measure because some strings are Strings, and some are Text.
class Unescaped s where
    toString :: s -> String

instance Unescaped String where
    toString = id

instance Unescaped Text where
    toString = T.unpack

instance Unescaped InterfaceName where
    toString = formatInterfaceName

instance Unescaped ObjectPath where
    toString = formatObjectPath

instance Unescaped MemberName where
    toString = formatMemberName

escape :: Unescaped s => s -> Marquee
escape = Marquee . escapeMarkup . toString

formatMember :: Maybe InterfaceName -> MemberName -> Marquee
formatMember iface member = iface' `mappend` b (escape member)
  where
    iface' = case iface of
        Just ifaceName -> escape ifaceName `mappend` Marquee "."
        Nothing        -> light (escape "(no interface) ")
