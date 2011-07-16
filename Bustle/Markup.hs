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
  , escape

  , formatMember
  )
where

import Data.Monoid

#if MIN_VERSION_gtk(0,11,0)
import Graphics.Rendering.Pango.Layout (escapeMarkup)
#else
import Graphics.UI.Gtk.Pango.Layout (escapeMarkup)
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

escape :: String -> Markup
escape = Markup . escapeMarkup

formatMember :: String -> String -> Markup
formatMember iface member = mconcat [ escape iface
                                    , escape "."
                                    , b (escape member)
                                    ]
