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

type NameStore = ListStore (Bool, (UniqueName, Set OtherName))

makeStore :: [(UniqueName, Set OtherName)]
          -> Set UniqueName
          -> IO NameStore
makeStore names currentlyHidden = do
    listStoreNew $ map toPair names
  where
    toPair (name@(u, _)) = (not (Set.member u currentlyHidden), name)

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

    return sw

runFilterDialog :: WindowClass parent
                => parent -- ^ The window to which to attach the dialog
                -> [(UniqueName, Set OtherName)] -- ^ Names, in order of appearance
                -> Set UniqueName -- ^ Currently-hidden names
                -> IO (Set UniqueName) -- ^ The set of names to *hide*
runFilterDialog parent names currentlyHidden = do
    d <- dialogNew
    windowSetTransientFor d parent
    dialogAddButton d stockClose ResponseClose
    vbox <- dialogGetUpper d
    boxSetSpacing vbox 6

    nameStore <- makeStore names currentlyHidden
    sw <- makeView nameStore

    instructions <- labelNew Nothing
    widgetSetSizeRequest instructions 600 (-1)
    labelSetMarkup instructions
        "Unticking a service hides its column in the diagram, \
        \and all messages it is involved in. That is, all methods it calls \
        \or are called on it, the corresponding returns, and all signals it \
        \emits will be hidden."
    labelSetLineWrap instructions True
    boxPackStart vbox instructions PackNatural 0

    containerAdd vbox sw
    widgetShowAll vbox

    _ <- dialogRun d

    widgetDestroy d

    results <- listStoreToList nameStore
    return $ Set.fromList [ u
                          | (ticked, (u, _)) <- results
                          , not ticked
                          ]
