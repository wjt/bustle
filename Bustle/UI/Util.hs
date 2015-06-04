{-
Bustle.UI.Util: miscellaneous clickable utility functions
Copyright © 2012 Collabora Ltd.

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
module Bustle.UI.Util
  (
    displayError
  )
where

import Graphics.UI.Gtk

import Bustle.Util (maybeM)

-- Displays a modal error dialog, with the given strings as title and body
-- respectively.
displayError :: Maybe Window
             -> String
             -> Maybe String
             -> IO ()
displayError mwindow title mbody = do
  dialog <- messageDialogNew mwindow
                             [DialogModal]
                             MessageError
                             ButtonsClose
                             title

  maybeM mbody $ messageDialogSetSecondaryText dialog

  dialog `after` response $ \_ -> widgetDestroy dialog
  widgetShowAll dialog
