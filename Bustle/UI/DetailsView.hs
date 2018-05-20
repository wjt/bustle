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
import Graphics.UI.Gtk hiding (Signal)

import qualified DBus as D

import Bustle.Types
import Bustle.Marquee
import Bustle.VariantFormatter

data DetailsView =
    DetailsView { detailsGrid :: Grid
                , detailsType :: Stack
                , detailsPath :: Label
                , detailsMember :: Label
                , detailsBodyView :: TextView
                }

detailsViewNew :: Builder
               -> IO DetailsView
detailsViewNew builder = do
    grid        <- builderGetObject builder castToGrid "detailsGrid"
    type_       <- builderGetObject builder castToStack "detailsType"
    pathLabel   <- builderGetObject builder castToLabel "detailsPath"
    memberLabel <- builderGetObject builder castToLabel "detailsMember"
    view        <- builderGetObject builder castToTextView "detailsArguments"

    return $ DetailsView grid type_ pathLabel memberLabel view

pickType :: Detailed Message -> String
pickType (Detailed _ m _ _) = case m of
    MethodCall {} -> "methodCall"
    MethodReturn {} -> "methodReturn"
    Error {} -> "error"
    Signal { signalDestination = d } ->
        case d of
            Nothing -> "signal"
            Just _  -> "directedSignal"

getMemberMarkup :: Member -> String
getMemberMarkup m =
    toPangoMarkup $ formatMember (iface m) (membername m)

getMember :: Detailed Message -> Maybe Member
getMember (Detailed _ m _ _) = case m of
    MethodCall {}   -> Just $ member m
    Signal {}       -> Just $ member m
    MethodReturn {} -> callMember
    Error {}        -> callMember
  where
    callMember = fmap (member . deEvent) $ inReplyTo m

formatMessage :: Detailed Message -> String
formatMessage (Detailed _ _ _ rm) =
    formatArgs $ D.receivedMessageBody rm
  where
    formatArgs = intercalate "\n" . map (format_Variant VariantStyleSignature)

detailsViewGetTop :: DetailsView -> Widget
detailsViewGetTop = toWidget . detailsGrid

detailsViewUpdate :: DetailsView
                  -> Detailed Message
                  -> IO ()
detailsViewUpdate d m = do
    buf <- textViewGetBuffer $ detailsBodyView d
    let member_ = getMember m
    stackSetVisibleChildName (detailsType d) (pickType m)
    labelSetText (detailsPath d) (maybe unknown (D.formatObjectPath . path) member_)
    labelSetMarkup (detailsMember d) (maybe unknown getMemberMarkup member_)
    textBufferSetText buf $ formatMessage m
  where
    unknown = ""
