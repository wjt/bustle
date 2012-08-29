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

setupOpenTwoDialog :: Builder
                   -> Window
                   -> (FilePath -> FilePath -> IO ())
                   -> IO Dialog
setupOpenTwoDialog builder parent callback = do
    dialog <- builderGetObject builder castToDialog "openTwoDialog"
    [sessionBusChooser, systemBusChooser] <-
        mapM (builderGetObject builder castToFileChooserButton)
            ["sessionBusChooser", "systemBusChooser"]
    openTwoOpenButton <- builderGetObject builder castToButton "openTwoOpenButton"

    windowSetTransientFor dialog parent
    dialog `on` deleteEvent $ tryEvent $ io $ widgetHide dialog

    propagateCurrentFolder sessionBusChooser systemBusChooser
    propagateCurrentFolder systemBusChooser sessionBusChooser

    let hideMyself = do
            widgetHideAll dialog
            fileChooserUnselectAll sessionBusChooser
            fileChooserUnselectAll systemBusChooser

    let updateOpenSensitivity = do
            sessionLogFile <- fileChooserGetFilename sessionBusChooser
            systemLogFile <- fileChooserGetFilename systemBusChooser

            widgetSetSensitive openTwoOpenButton $
              case (sessionLogFile, systemLogFile) of
                (Just _, Just _) -> True
                _                -> False
    connectGeneric "file-set" False sessionBusChooser updateOpenSensitivity
    connectGeneric "file-set" False systemBusChooser updateOpenSensitivity
    updateOpenSensitivity

    dialog `afterResponse` \resp -> do
      when (resp == ResponseAccept) $ do
          Just f1 <- fileChooserGetFilename sessionBusChooser
          Just f2 <- fileChooserGetFilename systemBusChooser
          callback f1 f2

      hideMyself

    return dialog
