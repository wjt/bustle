{-
Bustle.VariantFormatter: produces GVariant strings representing D-Bus values
Copyright © 2011 Will Thompson

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
module Bustle.VariantFormatter
  ( format_Variant
  , VariantStyle(..)
  )
where

import Data.Word
import Data.Int
import Data.List (intercalate)
-- :'(
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as Text

import DBus.Types

format_Bool :: Bool -> String
format_Bool = show

format_Word8 :: Word8 -> String
format_Word8 = show

format_Int16 :: Int16 -> String
format_Int16 = show
format_Int32 :: Int32 -> String
format_Int32 = show
format_Int64 :: Int64 -> String
format_Int64 = show

format_Word16 :: Word16 -> String
format_Word16 = show
format_Word32 :: Word32 -> String
format_Word32 = show
format_Word64 :: Word64 -> String
format_Word64 = show

format_Double :: Double -> String
format_Double = show

format_String :: String -> String
format_String = show

format_Signature :: Signature -> String
format_Signature = show . strSignature

format_ObjectPath :: ObjectPath -> String
format_ObjectPath = show . strObjectPath

format_Array :: Array -> String
format_Array a = "[" ++ intercalate ", " items ++ "]"
  where
    items = map (format_Variant VariantStyleBare) $ arrayItems a

format_Dictionary :: Dictionary -> String
format_Dictionary d = "{" ++ intercalate ", " items ++ "}"
  where
    items = map (\(k, v) -> format_Variant VariantStyleBare k ++ ": " ++ format_Variant VariantStyleBare v) $ dictionaryItems d


format_Structure :: Structure -> String
format_Structure (Structure []) = "()"
format_Structure (Structure [v]) = "(" ++ format_Variant VariantStyleBare v ++ ",)"
format_Structure (Structure vs) = "(" ++ intercalate ", " items ++ ")"
  where
    items = map (format_Variant VariantStyleBare) vs

data VariantStyle =
    VariantStyleBare
  | VariantStyleSignature
  | VariantStyleAngleBrackets

format_Variant :: VariantStyle -> Variant -> String
format_Variant style v =
    case style of
      VariantStyleBare -> formatted
      VariantStyleSignature -> typeSignature ++ " " ++ formatted
      VariantStyleAngleBrackets -> "<" ++ typeSignature ++ " " ++ formatted ++ ">"
  where
    ty = variantType v
    typeSignature = ('@':) . Text.unpack . typeCode $ ty
    format = case ty of
        DBusBoolean -> format_Bool . fromJust . fromVariant
        DBusByte -> format_Word8 . fromJust . fromVariant
        DBusInt16 -> format_Int16 . fromJust . fromVariant
        DBusInt32 -> format_Int32 . fromJust . fromVariant
        DBusInt64 -> format_Int64 . fromJust . fromVariant
        DBusWord16 -> format_Word16 . fromJust . fromVariant
        DBusWord32 -> format_Word32 . fromJust . fromVariant
        DBusWord64 -> format_Word64 . fromJust . fromVariant
        DBusDouble -> format_Double . fromJust . fromVariant
        DBusString -> format_String . fromJust . fromVariant
        DBusSignature -> format_Signature . fromJust . fromVariant
        DBusObjectPath -> format_ObjectPath . fromJust . fromVariant
        DBusVariant -> format_Variant VariantStyleAngleBrackets . fromJust . fromVariant
        DBusArray _ -> format_Array . fromJust . fromVariant
        DBusDictionary _ _ -> format_Dictionary . fromJust . fromVariant
        DBusStructure _ -> format_Structure . fromJust . fromVariant
    formatted = format v
