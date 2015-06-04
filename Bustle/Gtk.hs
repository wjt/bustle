{-
Bustle.Gtk: Stuff missing from the Gtk binding
Copyright © 2015 Will Thompson

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
{-# LANGUAGE ForeignFunctionInterface #-}
module Bustle.Gtk
  ( b_windowSetTitlebar
  , b_headerBarSetSubtitle
  )
where

import Foreign.Ptr
import Foreign.ForeignPtr

import qualified System.Glib.GObject as GO
import System.Glib.Properties (objectSetPropertyMaybeString)
import Graphics.UI.Gtk


foreign import ccall "gtk_window_set_titlebar"
    gtk_window_set_titlebar :: Ptr GObject
                    -> Ptr GObject
                    -> IO ()


b_windowSetTitlebar :: (WindowClass window,
                        WidgetClass widget)
                    => window
                    -> widget
                    -> IO ()
b_windowSetTitlebar window titlebar =
    withForeignPtr (GO.unGObject (GO.toGObject window)) $ \pWindow ->
    withForeignPtr (GO.unGObject (GO.toGObject titlebar)) $ \pTitlebar ->
    gtk_window_set_titlebar pWindow pTitlebar

b_headerBarSetSubtitle :: WidgetClass headerBar -- BZZT
                       => headerBar
                       -> Maybe String
                       -> IO ()
b_headerBarSetSubtitle = objectSetPropertyMaybeString "subtitle"
