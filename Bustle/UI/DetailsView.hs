module Bustle.UI.DetailsView
  ( DetailsView
  , detailsViewNew
  , detailsViewGetTop
  , detailsViewUpdate
  )
where

import Data.List (intercalate)
import Data.Maybe (maybe)
import Graphics.UI.Gtk hiding (Signal, Markup)
import qualified DBus.Message

import Bustle.Types
import Bustle.Markup

data DetailsView =
    DetailsView { detailsTable :: Table
                , detailsTitle :: Label
                , detailsPath :: Label
                , detailsMember :: Label
                , detailsBodyView :: TextView
                }

addTitle :: Table
         -> String
         -> Int
         -> IO ()
addTitle table title row = do
    label <- labelNew $ Just title
    miscSetAlignment label 0 0
    tableAttach table label 0 1 row (row + 1) [Fill] [Fill] 0 0

addValue :: Table
         -> Int
         -> IO Label
addValue table row = do
    label <- labelNew Nothing
    miscSetAlignment label 0 0
    labelSetEllipsize label EllipsizeStart
    labelSetSelectable label True
    tableAttach table label 1 2 row (row + 1) [Expand, Fill] [Fill] 0 0
    return label

addField :: Table
         -> String
         -> Int
         -> IO Label
addField table title row = do
    addTitle table title row
    addValue table row

detailsViewNew :: IO DetailsView
detailsViewNew = do
    table <- tableNew 2 3 False
    table `set` [ tableRowSpacing := 6
                , tableColumnSpacing := 6
                ]

    title <- labelNew Nothing
    miscSetAlignment title 0 0
    tableAttach table title 0 2 0 1 [Fill] [Fill] 0 0

    pathLabel <- addField table "Path:" 1
    memberLabel <- addField table "Member:" 2

    addTitle table "Arguments:" 3

    view <- textViewNew
    textViewSetWrapMode view WrapWordChar
    textViewSetEditable view False

    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
    containerAdd sw view

    tableAttachDefaults table sw 1 2 3 4

    widgetShowAll table
    return $ DetailsView table title pathLabel memberLabel view

pickTitle :: DetailedMessage -> Markup
pickTitle (DetailedMessage _ m _) = case m of
    MethodCall {} -> b (escape "Method call")
    MethodReturn {} -> b (escape "Method return")
    Error {} -> b (escape "Error")
    Signal {} -> b (escape "Signal")
    _ -> escape "I am made of chalk"

getMemberMarkup :: Member -> String
getMemberMarkup m =
    unMarkup $ formatMember (iface m) (membername m)

getMember :: DetailedMessage -> Maybe Member
getMember (DetailedMessage _ m _) = case m of
    MethodCall {}   -> Just $ member m
    Signal {}       -> Just $ member m
    MethodReturn {} -> fmap (member . dmMessage) $ inReplyTo m
    Error {}        -> fmap (member . dmMessage) $ inReplyTo m
    _               -> Nothing

formatMessage :: DetailedMessage -> String
formatMessage (DetailedMessage _ _ Nothing) =
    "# No message body information is available. Please capture a fresh log\n\
    \# using bustle-pcap if you need it!"
formatMessage (DetailedMessage _ _ (Just rm)) =
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
    let member_ = getMember m
    labelSetMarkup (detailsTitle d) (unMarkup $ pickTitle m)
    labelSetText (detailsPath d) (maybe "Unknown" path member_)
    labelSetMarkup (detailsMember d) (maybe "Unknown" getMemberMarkup member_)
    textBufferSetText buf $ formatMessage m
