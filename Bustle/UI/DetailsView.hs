{-
Bustle.UI.DetailsView: displays the bodies of D-Bus messages
Copyright © 2011–2012 Collabora Ltd.

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
module Bustle.UI.DetailsView
  ( DetailsView
  , detailsViewNew
  , detailsViewGetTop
  , detailsViewUpdate
  )
where

import Data.List (intercalate)
import Data.Maybe (maybe, fromJust)
import Control.Applicative ((<$>))
import Graphics.UI.Gtk hiding (Signal, Markup)
import qualified Data.Text as T
import qualified DBus.Message

import qualified DBus.Types as D

import Bustle.Types
import Bustle.Markup
import Bustle.VariantFormatter

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
    Signal { signalDestination = d } ->
        b . escape $ case d of
            Nothing -> "Signal"
            Just _  -> "Directed signal"
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
formatMessage (DetailedMessage _ _ (Just (_size, rm))) =
    formatArgs $ DBus.Message.receivedBody rm
  where
    formatArgs = intercalate "\n" . map (format_Variant VariantStyleSignature)

detailsViewGetTop :: DetailsView -> Widget
detailsViewGetTop = toWidget . detailsTable

detailsViewUpdate :: DetailsView
                  -> DetailedMessage
                  -> IO ()
detailsViewUpdate d m = do
    buf <- textViewGetBuffer $ detailsBodyView d
    let member_ = getMember m
    labelSetMarkup (detailsTitle d) (unMarkup $ pickTitle m)
    labelSetText (detailsPath d) (maybe "Unknown" (T.unpack . D.objectPathText . path) member_)
    labelSetMarkup (detailsMember d) (maybe "Unknown" getMemberMarkup member_)
    textBufferSetText buf $ formatMessage m
