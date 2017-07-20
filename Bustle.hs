{-
Bustle: a tool to draw charts of D-Bus activity
Copyright © 2008–2011 Collabora Ltd.

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
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main)
where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad (when)
import System.Glib.Utils (setApplicationName)
import Bustle.Noninteractive
import Bustle.Translation
import Bustle.UI

usage :: Bool
      -> IO ()
usage fatal = do
    hPutStrLn stderr
        "Usage:\n\
        \  bustle [LOGFILE [...]]\n\
        \  bustle --pair SESSION_LOGFILE SYSTEM_LOGFILE\n\
        \\n\
        \Or for batch-processing:\n\
        \  bustle --count LOGFILE\n\
        \  bustle --time LOGFILE\n\
        \  bustle --dot LOGFILE"
    when fatal exitFailure

runOne :: (String -> IO ())
       -> [String]
       -> IO ()
runOne f [filename] = f filename
runOne _ _ = usage True

main :: IO ()
main = do
    initTranslation
    setApplicationName (__ "Bustle")
    args <- getArgs

    case args of
        ["--help"]     -> usage False
        "--count":rest -> runOne runCount rest
        "--time":rest  -> runOne runTime rest
        "--dot":rest   -> runOne runDot rest
        _              -> uiMain

-- vim: sw=2 sts=2
