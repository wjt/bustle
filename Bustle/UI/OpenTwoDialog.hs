{-
Bustle.UI.OpenTwoDialog: a dialog to prompt the user to open two log files
Copyright © 2008–2012 Collabora Ltd.

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
module Bustle.UI.OpenTwoDialog
  (
    setupOpenTwoDialog
  )
where

import Data.Maybe (isJust, isNothing, fromJust)
import Control.Monad (when)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import Paths_bustle (getDataFileName)
import Bustle.Util

-- Propagates changes to d1's currently-selected folder to d2, if and only if
-- d2 doesn't have a currently-selected file (otherwise, choosing a file
-- from a different directory in the second chooser would unselect a
-- previously-selected file in the first).
propagateCurrentFolder :: FileChooserClass chooser
                       => chooser
                       -> chooser
                       -> IO (ConnectId chooser)
propagateCurrentFolder d1 d2 = d1 `onCurrentFolderChanged` do
    f1 <- fileChooserGetCurrentFolder d1
    f2 <- fileChooserGetCurrentFolder d2
    otherFile <- fileChooserGetFilename d2
    when (and [ isNothing otherFile
              , f1 /= f2
              , isJust f1
              ]) $ do
        fileChooserSetCurrentFolder d2 (fromJust f1)
        return ()

setupOpenTwoDialog :: Window
                   -> (FilePath -> FilePath -> IO ())
                   -> IO Dialog
setupOpenTwoDialog parent callback = do
    Just xml <- xmlNew =<< getDataFileName "data/bustle.glade"

    dialog <- xmlGetWidget xml castToDialog "openTwoDialog"
    [sessionBusChooser, systemBusChooser] <-
        mapM (xmlGetWidget xml castToFileChooserButton)
            ["sessionBusChooser", "systemBusChooser"]

    windowSetTransientFor dialog parent
    dialog `on` deleteEvent $ tryEvent $ io $ widgetHide dialog

    propagateCurrentFolder sessionBusChooser systemBusChooser
    propagateCurrentFolder systemBusChooser sessionBusChooser

    let hideMyself = do
            widgetHideAll dialog
            fileChooserUnselectAll sessionBusChooser
            fileChooserUnselectAll systemBusChooser

    dialog `afterResponse` \resp -> do
      -- The "Open" button should only be sensitive if both pickers have a
      -- file in them, but the GtkFileChooserButton:file-set signal is not
      -- bound in my version of Gtk2Hs. So yeah...
      if (resp == ResponseAccept)
        then do
          sessionLogFile <- fileChooserGetFilename sessionBusChooser
          systemLogFile <- fileChooserGetFilename systemBusChooser

          case (sessionLogFile, systemLogFile) of
            (Just f1, Just f2) -> do
                callback f1 f2
                hideMyself
            _ -> return ()
        else
          hideMyself

    return dialog
