module Bustle.Translation
    (
      initTranslation
    , __
    )
where

import Text.I18N.GetText
import System.Locale.SetLocale
import System.IO.Unsafe

import GetText_bustle

initTranslation :: IO ()
initTranslation = do
    setLocale LC_ALL (Just "")
    domain <- getMessageCatalogDomain
    dir <- getMessageCatalogDir
    bindTextDomain domain (Just dir)
    textDomain (Just domain)
    return ()

__ :: String -> String
-- FIXME: I do not like this unsafePerformIO one little bit.
__ = unsafePerformIO . getText
