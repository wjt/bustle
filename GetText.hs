-- | This library extends the Distribution with internationalization support.
--
-- It performs two functions:
--
-- * compiles and installs PO files to the specified directory
--
-- * tells the application where files were installed to make it able
-- to bind them to the code
--
-- Each PO file will be placed to the
-- @{datadir}\/locale\/{loc}\/LC_MESSAGES\/{domain}.mo@ where:
--
--  [@datadir@] Usually @prefix/share@ but could be different, depends
--  on system.
--
--  [@loc@] Locale name (language code, two characters). This module
--  supposes, that each PO file has a base name set to the proper
--  locale, e.g. @de.po@ is the German translation of the program, so
--  this file will be placed under @{datadir}\/locale\/de@ directory
--
--  [@domain@] Program domain. A unique identifier of single
--  translational unit (program). By default domain will be set to the
--  package name, but its name could be configured in the @.cabal@ file.
--
-- The module defines following @.cabal@ fields:
--
--  [@x-gettext-domain-name@] Name of the domain. One ofmore
--  alphanumeric characters separated by hyphens or underlines. When
--  not set, package name will be used.
--
--  [@x-gettext-po-files@] List of files with translations. Could be
--  used a limited form of wildcards, e.g.: @x-gettext-po-files:
--  po/*.po@
--
--  [@x-gettext-domain-def@] Name of the macro, in which domain name
--  will be passed to the program. Default value is
--  @__MESSAGE_CATALOG_DOMAIN__@
--
--  [@x-gettext-msg-cat-def@] Name of the macro, in which path to the
--  message catalog will be passed to the program. Default value is
--  @__MESSAGE_CATALOG_DIR__@
--
-- The last two parameters are used to send configuration data to the
-- code during its compilation. The most common usage example is:
--
--
-- > ...
-- > prepareI18N = do
-- >    setLocale LC_ALL (Just "") 
-- >    bindTextDomain __MESSAGE_CATALOG_DOMAIN__ (Just __MESSAGE_CATALOG_DIR__)
-- >    textDomain __MESSAGE_CATALOG_DOMAIN__
-- >
-- > main = do
-- >    prepareI18N
-- >    ...
-- >
-- > ...
--
--
-- /NOTE:/ files, passed in the @x-gettext-po-files@ are not
-- automatically added to the source distribution, so they should be
-- also added to the @extra-source-files@ parameter, along with
-- translation template file (usually @message.pot@)
--
-- /WARNING:/ sometimes, when only configuration targets changes, code
-- will not recompile, thus you should execute @cabal clean@ to
-- cleanup the build and restart it again from the configuration. This
-- is temporary bug, it will be fixed in next releases.
--
-- /TODO:/ this is lifted verbatim (modulo other /TODO/s) from hgettext's
-- Distribution.Simple.I18N.GetText partly to expose individual hooks and
-- partly to avoid the /cabal configure/-time dependency. For the latter,
-- see https://github.com/fpco/stackage/issues/746
-- 

module GetText
    (
    -- | /TODO:/ upstream exporting the individual hooks?
     installPOFiles,

    -- | /TODO:/ upstream generating GetText_foo.hs rather than exporting these?
     getDomainNameDefault,
     getPackageName,
     targetDataDir,

     installGetTextHooks,
     gettextDefaultMain
    ) where

import Distribution.Simple
import Distribution.Simple.Setup as S
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.Configure
import Distribution.Simple.InstallDirs as I
import Distribution.Simple.Utils

import Language.Haskell.Extension

import Control.Monad
import Control.Arrow (second)
import Data.Maybe (listToMaybe, maybeToList, fromMaybe)
import Data.List (unfoldr,nub,null)
import System.FilePath
import System.Directory
import System.Process

-- | Default main function, same as
-- 
-- > defaultMainWithHooks $ installGetTextHooks simpleUserHooks
-- 
gettextDefaultMain :: IO ()
gettextDefaultMain = defaultMainWithHooks $ installGetTextHooks simpleUserHooks

-- | Installs hooks, used by GetText module to install
-- PO files to the system. Previous won't be disabled
--
installGetTextHooks :: UserHooks -- ^ initial user hooks
                    -> UserHooks -- ^ patched user hooks
installGetTextHooks uh = uh{
                           confHook = \a b ->
                                      updateLocalBuildInfo <$> (confHook uh) a b,

                           postInst = \a b c d ->
                                      postInst uh a b c d >>
                                      installPOFiles a b c d
                         }


updateLocalBuildInfo :: LocalBuildInfo -> LocalBuildInfo
updateLocalBuildInfo l =
    let sMap = getCustomFields l
        [domDef, catDef] = map ($ sMap) [getDomainDefine, getMsgCatalogDefine]
        dom = getDomainNameDefault sMap (getPackageName l)
        tar = targetDataDir l
        [catMS, domMS] = map (uncurry formatMacro) [(domDef, dom), (catDef, tar)]
    in (appendCPPOptions [domMS,catMS] . appendExtension [EnableExtension CPP]) l

installPOFiles :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
installPOFiles _ _ _ l =
    let sMap = getCustomFields l
        destDir = targetDataDir l
        dom = getDomainNameDefault sMap (getPackageName l)
        installFile file = do
          let fname = takeFileName file
          let bname = takeBaseName fname
          let targetDir = destDir </> bname </> "LC_MESSAGES"
          -- ensure we have directory destDir/{loc}/LC_MESSAGES
          createDirectoryIfMissing True targetDir
          system $ "msgfmt --output-file=" ++
                     (targetDir </> dom <.> "mo") ++
                     " " ++ file
    in do
      filelist <- getPoFilesDefault sMap
      -- copy all whose name is in the form of dir/{loc}.po to the
      -- destDir/{loc}/LC_MESSAGES/dom.mo
      -- with the 'msgfmt' tool
      mapM_ installFile filelist

forBuildInfo :: LocalBuildInfo -> (BuildInfo -> BuildInfo) -> LocalBuildInfo
forBuildInfo l f =
    let a = l{localPkgDescr = updPkgDescr (localPkgDescr l)}
        updPkgDescr x = x{library = updLibrary (library x),
                          executables = updExecs (executables x)}
        updLibrary Nothing = Nothing
        updLibrary (Just x) = Just $ x{libBuildInfo = f (libBuildInfo x)}
        updExecs x = map updExec x
        updExec x = x{buildInfo = f (buildInfo x)}
    in a

appendExtension :: [Extension] -> LocalBuildInfo -> LocalBuildInfo
appendExtension exts l =
    forBuildInfo l updBuildInfo
    where updBuildInfo x = x{defaultExtensions = updExts (defaultExtensions x)}
          updExts s = nub (s ++ exts)

appendCPPOptions :: [String] -> LocalBuildInfo -> LocalBuildInfo
appendCPPOptions opts l =
    forBuildInfo l updBuildInfo
    where updBuildInfo x = x{cppOptions = updOpts (cppOptions x)}
          updOpts s = nub (s ++ opts)

formatMacro name value = "-D" ++ name ++ "=" ++ show value

targetDataDir :: LocalBuildInfo -> FilePath
targetDataDir l =
    let dirTmpls = installDirTemplates l
        prefix' = prefix dirTmpls
        data' = datadir dirTmpls
        dataEx = I.fromPathTemplate $ I.substPathTemplate [(PrefixVar, prefix')] data'
    in dataEx ++ "/locale"

getPackageName :: LocalBuildInfo -> String
getPackageName = unPackageName . packageName . localPkgDescr

getCustomFields :: LocalBuildInfo -> [(String, String)]
getCustomFields = customFieldsPD . localPkgDescr

findInParametersDefault :: [(String, String)] -> String -> String -> String
findInParametersDefault al name def = (fromMaybe def . lookup name) al

getDomainNameDefault :: [(String, String)] -> String -> String
getDomainNameDefault al d = findInParametersDefault al "x-gettext-domain-name" d

getDomainDefine :: [(String, String)] -> String
getDomainDefine al = findInParametersDefault al "x-gettext-domain-def" "__MESSAGE_CATALOG_DOMAIN__"

getMsgCatalogDefine :: [(String, String)] -> String
getMsgCatalogDefine al = findInParametersDefault al "x-gettext-msg-cat-def" "__MESSAGE_CATALOG_DIR__"

getPoFilesDefault :: [(String, String)] -> IO [String]
getPoFilesDefault al = toFileList $ findInParametersDefault al "x-gettext-po-files" ""
    where toFileList "" = return []
          toFileList x = fmap concat $ mapM matchFileGlob $ split' x
          -- from Blow your mind (HaskellWiki)
          -- splits string by newline, space and comma
          split' x = concatMap lines $ concatMap words $ unfoldr (\b -> fmap (const . second (drop 1) . break (==',') $ b) . listToMaybe $ b) x

