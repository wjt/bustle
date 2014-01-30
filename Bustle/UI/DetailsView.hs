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
import Graphics.UI.Gtk hiding (Signal, Markup)

import qualified DBus as D

import Bustle.Translation (__)
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

    pathLabel <- addField table (__ "Path:") 1
    memberLabel <- addField table (__ "Member:") 2

    addTitle table (__ "Arguments:") 3

    view <- textViewNew
    textViewSetWrapMode view WrapWordChar
    textViewSetEditable view False

    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
    containerAdd sw view

    tableAttachDefaults table sw 1 2 3 4

    widgetShowAll table
    return $ DetailsView table title pathLabel memberLabel view

pickTitle :: Detailed Message -> Markup
pickTitle (Detailed _ m _) = case m of
    MethodCall {} -> b (escape (__ "Method call"))
    MethodReturn {} -> b (escape (__ "Method return"))
    Error {} -> b (escape (__ "Error"))
    Signal { signalDestination = d } ->
        b . escape $ case d of
            Nothing -> (__ "Signal")
            Just _  -> (__ "Directed signal")

getMemberMarkup :: Member -> String
getMemberMarkup m =
    unMarkup $ formatMember (iface m) (membername m)

getMember :: Detailed Message -> Maybe Member
getMember (Detailed _ m _) = case m of
    MethodCall {}   -> Just $ member m
    Signal {}       -> Just $ member m
    MethodReturn {} -> callMember
    Error {}        -> callMember
  where
    callMember = fmap (member . deEvent) $ inReplyTo m

formatMessage :: Detailed Message -> String
formatMessage (Detailed _ _ Nothing) =
    __ "No message body information is available. Please capture a fresh log \
       \using a recent version of Bustle!"
formatMessage (Detailed _ _ (Just (_size, rm))) =
    formatArgs $ D.receivedMessageBody rm
  where
    formatArgs = intercalate "\n" . map (format_Variant VariantStyleSignature)

detailsViewGetTop :: DetailsView -> Widget
detailsViewGetTop = toWidget . detailsTable

detailsViewUpdate :: DetailsView
                  -> Detailed Message
                  -> IO ()
detailsViewUpdate d m = do
    buf <- textViewGetBuffer $ detailsBodyView d
    let member_ = getMember m
    labelSetMarkup (detailsTitle d) (unMarkup $ pickTitle m)
    labelSetText (detailsPath d) (maybe unknown (D.formatObjectPath . path) member_)
    labelSetMarkup (detailsMember d) (maybe unknown getMemberMarkup member_)
    textBufferSetText buf $ formatMessage m
  where
    unknown = __ "<unknown>"
