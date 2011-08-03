module Bustle.UI.FilterDialog
  ( runFilterDialog
  )
where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Graphics.UI.Gtk

import Bustle.Types

runFilterDialog :: WindowClass parent
                => parent
                -> Map UniqueName (Set OtherName)
                -> IO ()
runFilterDialog parent names = do
    d <- dialogNew
    windowSetTransientFor d parent
    -- FIXME: not really kosher, seems needed for Gnome 3.0 to do the cool sheet thing
    --windowSetModal dialog True

    _ <- dialogRun d

    widgetDestroy d

    return ()
