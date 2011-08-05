{-
Bustle.Diagram: My First Type-Safe Markup Library
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
{-# LANGUAGE CPP #-}
module Bustle.Markup
  ( Markup
  , unMarkup
  , tag
  , b
  , i
  , red
  , a
  , escape

  , formatMember
  )
where

import Data.Monoid

#if MIN_VERSION_gtk(0,11,0)
import Graphics.Rendering.Pango.BasicTypes (Weight(..))
import Graphics.Rendering.Pango.Layout (escapeMarkup)
import Graphics.Rendering.Pango.Markup (markSpan, SpanAttribute(..))
#else
import Graphics.UI.Gtk.Pango.BasicTypes (Weight(..))
import Graphics.UI.Gtk.Pango.Layout (escapeMarkup)
import Graphics.UI.Gtk.Pango.Markup (markSpan, SpanAttribute(..))
#endif

newtype Markup = Markup { unMarkup :: String }
    deriving (Show, Read, Ord, Eq)

instance Monoid Markup where
    mempty = Markup ""
    mappend x y = Markup (unMarkup x `mappend` unMarkup y)
    mconcat = Markup . mconcat . map unMarkup

--raw :: String -> Markup
--raw = Markup

tag :: String -> Markup -> Markup
tag name contents =
    Markup $ concat [ "<", name, ">"
                    , unMarkup contents
                    , "</", name, ">"
                    ]

b, i :: Markup -> Markup
b = tag "b"
i = tag "i"

a :: String
  -> String
  -> Markup
a href contents =
  Markup $ concat [ "<a href=\"", escapeMarkup href, "\">"
                  , escapeMarkup contents
                  , "</a>"
                  ]

span_ :: [SpanAttribute] -> Markup -> Markup
span_ attrs = Markup . markSpan attrs . unMarkup

light :: Markup -> Markup
light = span_ [FontWeight WeightLight]

red :: Markup -> Markup
red = span_ [FontForeground "#ff0000"]

escape :: String -> Markup
escape = Markup . escapeMarkup

formatMember :: Maybe String -> String -> Markup
formatMember iface member = iface' `mappend` b (escape member)
  where
    iface' = case iface of
        Just ifaceName -> escape $ ifaceName ++ "."
        Nothing        -> light (escape "(no interface) ")
