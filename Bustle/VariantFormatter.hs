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
import Data.Char (chr, isPrint)
-- :'(
import Data.Maybe (fromJust)

import DBus

format_Bool :: Bool -> String
format_Bool = show

format_Word8 :: Word8 -> String
format_Word8 = show

format_ByteArray :: Array -> String
format_ByteArray ay =
    if all isPrintish chars
        then 'b':show chars
        else format_Array ay
  where
    bytes = map (fromJust . fromVariant) (arrayItems ay) :: [Word8]
    chars = map (chr . fromIntegral) bytes
    isPrintish '\0' = True
    isPrintish c    = isPrint c


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
format_Signature = show . formatSignature

format_ObjectPath :: ObjectPath -> String
format_ObjectPath = show . formatObjectPath

format_Array :: Array -> String
format_Array a = "[" ++ intercalate ", " items ++ "]"
  where
    items = map (format_Variant VariantStyleBare) $ arrayItems a

format_Dictionary :: Dictionary -> String
format_Dictionary d = "{" ++ intercalate ", " items ++ "}"
  where
    items = map (\(k, v) -> format_Variant VariantStyleBare k ++ ": " ++ format_Variant VariantStyleBare v) $ dictionaryItems d

-- FIXME…
format_Structure :: Structure -> String
format_Structure s = case structureItems s of
    []  -> "()"
    [v] -> "(" ++ format_Variant VariantStyleBare v ++ ",)"
    vs  -> "(" ++ intercalate ", " items ++ ")"
      where
        items = map (format_Variant VariantStyleBare) vs

data VariantStyle =
    VariantStyleBare
  | VariantStyleSignature
  | VariantStyleAngleBrackets

-- why did you remove typeCode from the public API, John…
typeCode :: Type -> String
typeCode TypeBoolean    = "b"
typeCode TypeWord8      = "y"
typeCode TypeWord16     = "q"
typeCode TypeWord32     = "u"
typeCode TypeWord64     = "t"
typeCode TypeInt16      = "n"
typeCode TypeInt32      = "i"
typeCode TypeInt64      = "x"
typeCode TypeDouble     = "d"
typeCode TypeString     = "s"
typeCode TypeSignature  = "g"
typeCode TypeObjectPath = "o"
typeCode TypeUnixFd     = "h"
typeCode TypeVariant    = "v"
typeCode (TypeArray t)  = 'a':typeCode t
typeCode (TypeDictionary kt vt) = concat [ "a{", typeCode kt , typeCode vt, "}"]
typeCode (TypeStructure ts) = concat ["(", concatMap typeCode ts, ")"]

format_Variant :: VariantStyle -> Variant -> String
format_Variant style v =
    case style of
      VariantStyleBare -> formatted
      VariantStyleSignature -> typeSignature ++ " " ++ formatted
      VariantStyleAngleBrackets -> "<" ++ typeSignature ++ " " ++ formatted ++ ">"
  where
    ty = variantType v
    typeSignature = ('@':) . typeCode $ ty
    format = case ty of
        TypeBoolean -> format_Bool . fromJust . fromVariant
        TypeInt16 -> format_Int16 . fromJust . fromVariant
        TypeInt32 -> format_Int32 . fromJust . fromVariant
        TypeInt64 -> format_Int64 . fromJust . fromVariant
        TypeWord8 -> format_Word8 . fromJust . fromVariant
        TypeWord16 -> format_Word16 . fromJust . fromVariant
        TypeWord32 -> format_Word32 . fromJust . fromVariant
        TypeWord64 -> format_Word64 . fromJust . fromVariant
        TypeDouble -> format_Double . fromJust . fromVariant
        TypeString -> format_String . fromJust . fromVariant
        TypeSignature -> format_Signature . fromJust . fromVariant
        TypeObjectPath -> format_ObjectPath . fromJust . fromVariant
        TypeUnixFd -> const "<fd>"
        TypeVariant -> format_Variant VariantStyleAngleBrackets . fromJust . fromVariant
        TypeArray TypeWord8 -> format_ByteArray . fromJust . fromVariant
        TypeArray _ -> format_Array . fromJust . fromVariant
        TypeDictionary _ _ -> format_Dictionary . fromJust . fromVariant
        TypeStructure _ -> format_Structure . fromJust . fromVariant
    formatted = format v
