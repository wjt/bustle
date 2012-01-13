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

  dialog `afterResponse` \_ -> widgetDestroy dialog
  widgetShowAll dialog
