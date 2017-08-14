{-
Bustle.UI.RecordAddressDialog: a dialog to prompt the user to open two log files
Copyright © 2018 Will Thompson

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
module Bustle.UI.RecordAddressDialog
  (
    showRecordAddressDialog
  )
where

import Control.Monad (when)

import Data.Text (Text)
import Graphics.UI.Gtk
import System.Glib.UTFString (stringLength)

import Paths_bustle

showRecordAddressDialog :: Window
                        -> (String -> IO ())
                        -> IO ()
showRecordAddressDialog parent callback = do
    builder <- builderNew
    builderAddFromFile builder =<< getDataFileName "data/RecordAddressDialog.ui"

    dialog <- builderGetObject builder castToDialog "recordAddressDialog"
    entry <- builderGetObject builder castToEntry "recordAddressEntry"
    record <- builderGetObject builder castToButton "recordAddressRecord"

    dialog `set` [ windowTransientFor := parent ]
    entry `on` editableChanged $ do
        address <- entryGetText entry
        -- TODO: validate with g_dbus_is_suppported_address() once
        -- https://gitlab.gnome.org/GNOME/glib/merge_requests/103 is in a
        -- release.
        widgetSetSensitive record (stringLength (address :: Text) /= 0)

    dialog `after` response $ \resp -> do
        when (resp == ResponseAccept) $ do
            address <- entryGetText entry
            callback address

        widgetDestroy dialog

    widgetShowAll dialog
