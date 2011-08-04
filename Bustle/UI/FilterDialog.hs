module Bustle.UI.FilterDialog
  ( runFilterDialog
  )
where

import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad (liftM)

import Graphics.UI.Gtk

import Bustle.Types

formatNames :: (UniqueName, Set OtherName)
            -> String
formatNames (u, os)
    | Set.null os = unUniqueName u
    | otherwise = intercalate "\n" . map unOtherName $ Set.toAscList os

makeStore :: [(UniqueName, Set OtherName)]
          -> Set UniqueName
          -> IO (ListStore (Bool, (UniqueName, Set OtherName)))
makeStore names currentlyHidden = do
    listStoreNew $ map toPair names
  where
    toPair (name@(u, _)) = (not (Set.member u currentlyHidden), name)

runFilterDialog :: WindowClass parent
                => parent
                -> [(UniqueName, Set OtherName)]
                -> Set UniqueName
                -> IO (Set UniqueName) -- ^ The set of names to *hide*
runFilterDialog parent names currentlyHidden = do
    d <- dialogNew
    windowSetTransientFor d parent

    dialogAddButton d stockClose ResponseClose

    nameStore <- makeStore names currentlyHidden

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

    onCellToggled tickyCell $ \pathstr -> do
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

    vbox <- dialogGetUpper d
    containerAdd vbox sw
    widgetShowAll sw

    _ <- dialogRun d

    widgetDestroy d

    results <- listStoreToList nameStore
    return $ Set.fromList [ u
                          | (ticked, (u, _)) <- results
                          , not ticked
                          ]
