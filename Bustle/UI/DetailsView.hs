module Bustle.UI.DetailsView
  ( DetailsView
  , detailsViewNew
  , detailsViewGetTop
  , detailsViewUpdate
  )
where

import Data.List (intercalate)
import Graphics.UI.Gtk
import qualified DBus.Message
import Bustle.Types

data DetailsView =
    DetailsView { detailsTable :: Table
                , detailsBodyView :: TextView
                }

detailsViewNew :: IO DetailsView
detailsViewNew = do
    table <- tableNew 1 1 False

    view <- textViewNew
    textViewSetWrapMode view WrapWordChar

    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
    containerAdd sw view

    tableAttach table sw 0 1 0 1 [Expand, Fill] [Expand, Fill] 0 0
    widgetShowAll sw

    return $ DetailsView table view

formatMessage :: DetailedMessage -> String
formatMessage (DetailedMessage _ Nothing) =
    "# No message body information is available. Please capture a fresh log\n\
    \# using bustle-pcap if you need it!"
formatMessage (DetailedMessage _ (Just rm)) =
    formatArgs $ DBus.Message.receivedBody rm
  where
    formatArgs = intercalate "\n\n" . map show


detailsViewGetTop :: DetailsView -> Widget
detailsViewGetTop = toWidget . detailsTable

detailsViewUpdate :: DetailsView
                  -> DetailedMessage
                  -> IO ()
detailsViewUpdate d m = do
    buf <- textViewGetBuffer $ detailsBodyView d
    textBufferSetText buf $ formatMessage m
