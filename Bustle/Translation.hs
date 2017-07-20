{-# LANGUAGE CPP #-}
module Bustle.Translation
    (
      initTranslation
    , __
    )
where

#ifdef USE_HGETTEXT
import Text.I18N.GetText
import System.Locale.SetLocale
import System.IO.Unsafe

import GetText_bustle
#endif

initTranslation :: IO ()
__ :: String -> String

#ifdef USE_HGETTEXT
initTranslation = do
    setLocale LC_ALL (Just "")
    domain <- getMessageCatalogDomain
    dir <- getMessageCatalogDir
    bindTextDomain domain (Just dir)
    textDomain (Just domain)
    return ()

-- FIXME: I do not like this unsafePerformIO one little bit.
__ = unsafePerformIO . getText
#else
initTranslation = return ()
__ = id
#endif