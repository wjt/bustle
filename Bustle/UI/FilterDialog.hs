{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-
Bustle.UI.FilterDialog: allows the user to filter the displayed log
Copyright © 2011 Collabora Ltd.

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
module Bustle.UI.FilterDialog
  ( runFilterDialog
  )
where

import Data.List (intercalate, groupBy, elemIndices)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Function as F

import Graphics.UI.Gtk

import Bustle.Translation (__)
import Bustle.Types

namespace :: String
          -> (String, String)
namespace name = case reverse (elemIndices '.' name) of
    []    -> ("", name)
    (i:_) -> splitAt (i + 1) name

formatNames :: (UniqueName, Set OtherName)
            -> String
formatNames (u, os)
    | Set.null os = unUniqueName u
    | otherwise = intercalate "\n" . map (formatGroup . groupGroup) $ groups
  where
    groups = groupBy ((==) `F.on` fst) . map (namespace . unOtherName) $ Set.toAscList os

    groupGroup [] = error "unpossible empty group from groupBy"
    groupGroup xs@((ns, _):_) = (ns, map snd xs)

    formatGroup (ns, [y]) = ns ++ y
    formatGroup (ns, ys)  = ns ++ "{" ++ intercalate "," ys ++ "}"

type NameStore = ListStore (Bool, (UniqueName, Set OtherName))

makeStore :: [(UniqueName, Set OtherName)]
          -> Set UniqueName
          -> IO NameStore
makeStore names currentlyHidden =
    listStoreNew $ map toPair names
  where
    toPair name@(u, _) = (not (Set.member u currentlyHidden), name)

makeView :: NameStore
         -> IO ScrolledWindow
makeView nameStore = do
    nameView <- treeViewNewWithModel nameStore
    -- We want rules because otherwise it's tough to see where each group
    -- starts and ends
    treeViewSetRulesHint nameView True
    treeViewSetHeadersVisible nameView False
    widgetSetSizeRequest nameView 600 371

    tickyCell <- cellRendererToggleNew
    tickyColumn <- treeViewColumnNew
    treeViewColumnPackStart tickyColumn tickyCell True
    treeViewAppendColumn nameView tickyColumn

    cellLayoutSetAttributes tickyColumn tickyCell nameStore $ \(ticked, _) ->
        [ cellToggleActive := ticked ]

    on tickyCell cellToggled $ \pathstr -> do
        let [i] = stringToTreePath pathstr
        (v, ns) <- listStoreGetValue nameStore i
        listStoreSetValue nameStore i (not v, ns)

    nameCell <- cellRendererTextNew
    nameColumn <- treeViewColumnNew
    treeViewColumnPackStart nameColumn nameCell True
    treeViewAppendColumn nameView nameColumn

    cellLayoutSetAttributes nameColumn nameCell nameStore $ \(_, ns) ->
        [ cellText := formatNames ns ]

    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
    containerAdd sw nameView

    return sw

runFilterDialog :: WindowClass parent
                => parent -- ^ The window to which to attach the dialog
                -> [(UniqueName, Set OtherName)] -- ^ Names, in order of appearance
                -> Set UniqueName -- ^ Currently-hidden names
                -> IO (Set UniqueName) -- ^ The set of names to *hide*
runFilterDialog parent names currentlyHidden = do
    d <- dialogNew
    (windowWidth, windowHeight) <- windowGetSize parent
    windowSetDefaultSize d (windowWidth * 7 `div` 8) (windowHeight `div` 2)
    d `set` [ windowTransientFor := parent ]
    dialogAddButton d stockClose ResponseClose
    vbox <- castToBox <$> dialogGetContentArea d
    boxSetSpacing vbox 6

    nameStore <- makeStore names currentlyHidden
    sw <- makeView nameStore

    instructions <- labelNew (Nothing :: Maybe String)
    widgetSetSizeRequest instructions 600 (-1)
    labelSetMarkup instructions
        (__ "Unticking a service hides its column in the diagram, \
        \and all messages it is involved in. That is, all methods it calls \
        \or are called on it, the corresponding returns, and all signals it \
        \emits will be hidden.")
    labelSetLineWrap instructions True
    boxPackStart vbox instructions PackNatural 0

    boxPackStart vbox sw PackGrow 0
    widgetShowAll vbox

    _ <- dialogRun d

    widgetDestroy d

    results <- listStoreToList nameStore
    return $ Set.fromList [ u
                          | (ticked, (u, _)) <- results
                          , not ticked
                          ]
