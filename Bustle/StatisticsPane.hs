{-
Bustle.StatisticsPane: implementation of the stats pane
Copyright © 2010–011 Collabora Ltd.

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
module Bustle.StatisticsPane
  ( StatsPane
  , statsPaneNew
  , statsPaneSetMessages
  )
where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Text.Printf
import Graphics.UI.Gtk hiding (Markup)
import Bustle.Stats
import Bustle.Types (Log)
import qualified Bustle.Markup as Markup
import Bustle.Markup (Markup)
import Data.Monoid

data StatsPane =
    StatsPane { spCountStore :: ListStore FrequencyInfo
              , spTimeStore :: ListStore TimeInfo
              , spSizeStore :: ListStore SizeInfo
              }

statsPaneNew :: Builder
             -> Maybe Pixbuf
             -> Maybe Pixbuf
             -> IO StatsPane
statsPaneNew builder methodIcon signalIcon = do
  [frequencySW, durationSW, sizeSW] <- mapM (builderGetObject builder castToScrolledWindow)
      ["frequencySW", "durationSW", "sizeSW"]

  (countStore, countView) <- newCountView methodIcon signalIcon
  containerAdd frequencySW countView

  (timeStore, timeView) <- newTimeView
  containerAdd durationSW timeView

  (sizeStore, sizeView) <- newSizeView methodIcon signalIcon
  containerAdd sizeSW sizeView

  widgetShow countView
  widgetShow timeView
  widgetShow sizeView

  return $ StatsPane countStore timeStore sizeStore

statsPaneSetMessages :: StatsPane
                     -> Log -- ^ session bus messages
                     -> Log -- ^ system bus messages
                     -> IO ()
statsPaneSetMessages sp sessionMessages systemMessages = do
    -- This conflates messages on the system bus and on the session bus,
    -- but I think that's okay for now.
    let allMessages = sessionMessages ++ systemMessages

    listStoreClear (spCountStore sp)
    listStoreClear (spTimeStore sp)
    listStoreClear (spSizeStore sp)

    forM_ (frequencies allMessages) $ listStoreAppend (spCountStore sp)
    forM_ (methodTimes allMessages) $ listStoreAppend (spTimeStore sp)
    forM_ (messageSizes allMessages) $ listStoreAppend (spSizeStore sp)

addTextRenderer :: TreeViewColumn
                -> ListStore a
                -> Bool
                -> (a -> Markup)
                -> IO CellRendererText
addTextRenderer col store expand f = do
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer expand
    set renderer [ cellTextSizePoints := 7 ]
    cellLayoutSetAttributes col renderer store $ \x ->
        [ cellTextMarkup := Just . Markup.unMarkup $ f x ]
    return renderer

addMemberRenderer :: TreeViewColumn
                  -> ListStore a
                  -> Bool
                  -> (a -> Markup)
                  -> IO CellRendererText
addMemberRenderer col store expand f = do
    renderer <- addTextRenderer col store expand f
    set renderer [ cellTextEllipsize := EllipsizeStart
                 , cellTextEllipsizeSet := True
                 , cellXAlign := 1
                 , cellTextWidthChars := 30
                 ]
    return renderer

addStatColumn :: TreeView
              -> ListStore a
              -> String
              -> (a -> Markup)
              -> IO ()
addStatColumn view store title f = do
    col <- treeViewColumnNew
    treeViewColumnSetTitle col title
    renderer <- addTextRenderer col store True f
    set renderer [ cellXAlign := 1 ]
    treeViewAppendColumn view col
    return ()

addTextStatColumn :: TreeView
                  -> ListStore a
                  -> String
                  -> (a -> String)
                  -> IO ()
addTextStatColumn view store title f =
    addStatColumn view store title (Markup.escape . f)

-- If we managed to load the method and signal icons...
maybeAddTypeIconColumn :: CellLayoutClass layout
                       => layout
                       -> ListStore a
                       -> Maybe Pixbuf
                       -> Maybe Pixbuf
                       -> (a -> Bool)
                       -> IO ()
maybeAddTypeIconColumn nameColumn store (Just m) (Just s) isMethod = do
    typeRenderer <- cellRendererPixbufNew
    cellLayoutPackStart nameColumn typeRenderer False
    cellLayoutSetAttributes nameColumn typeRenderer store $ \row ->
            [ cellPixbuf := if isMethod row then m else s ]
maybeAddTypeIconColumn _ _ _ _ _ = return ()

newCountView :: Maybe Pixbuf
             -> Maybe Pixbuf
             -> IO (ListStore FrequencyInfo, TreeView)
newCountView method signal = do
  countStore <- listStoreNew []
  countView <- treeViewNewWithModel countStore

  set countView [ treeViewHeadersVisible := False ]

  nameColumn <- treeViewColumnNew
  treeViewColumnSetTitle nameColumn "Name"
  set nameColumn [ treeViewColumnResizable := True
                 , treeViewColumnExpand := True
                 ]

  maybeAddTypeIconColumn nameColumn countStore method signal $ \fi ->
      case fiType fi of
          TallyMethod -> True
          TallySignal -> False

  addMemberRenderer nameColumn countStore True $ \fi ->
      Markup.formatMember (fiInterface fi) (fiMember fi)
  treeViewAppendColumn countView nameColumn

  countColumn <- treeViewColumnNew
  treeViewColumnSetTitle countColumn "Frequency"
  treeViewColumnSetMinWidth countColumn 120

  -- Using a progress bar here is not really ideal, but I CBA to do anything
  -- more auspicious right now. :)
  countBar <- cellRendererProgressNew
  cellLayoutPackStart countColumn countBar True
  cellLayoutSetAttributes countColumn countBar countStore $
      \(FrequencyInfo {fiFrequency = count}) ->
      [ cellProgressValue :=> do
          upperBound <- maximum . map fiFrequency <$>
                        listStoreToList countStore
          -- ensure that we always show *something*
          return $ 2 + (count * 98 `div` upperBound)
      , cellProgressText := Just $ show count
      ]

  treeViewAppendColumn countView countColumn

  return (countStore, countView)

newTimeView :: IO (ListStore TimeInfo, TreeView)
newTimeView = do
  timeStore <- listStoreNew []
  timeView <- treeViewNewWithModel timeStore

  set timeView [ treeViewHeadersVisible := True ]

  nameColumn <- treeViewColumnNew
  treeViewColumnSetTitle nameColumn "Method"
  set nameColumn [ treeViewColumnResizable := True
                 , treeViewColumnExpand := True
                 ]

  addMemberRenderer nameColumn timeStore True $ \ti ->
      Markup.formatMember (tiInterface ti) (tiMethodName ti)
  treeViewAppendColumn timeView nameColumn

  addTextStatColumn timeView timeStore "Total"
                (printf "%.1f ms" . tiTotalTime)
  addTextStatColumn timeView timeStore "Calls" (show . tiCallFrequency)
  addTextStatColumn timeView timeStore "Mean"
                (printf "%.1f ms" . tiMeanCallTime)

  return (timeStore, timeView)

formatSizeInfoMember :: SizeInfo -> Markup
formatSizeInfoMember si =
    f (Markup.formatMember (siInterface si) (siName si))
  where
    f = case siType si of
            SizeReturn -> Markup.i
            SizeError  -> Markup.red
            _          -> id

formatSize :: Int -> Markup
formatSize s
    | s < maxB = value 1 `mappend` units "B"
    | s < maxKB = value 1024 `mappend` units "KB"
    | otherwise = value (1024 * 1024) `mappend` units "MB"
  where
    maxB = 10000
    maxKB = 10000 * 1024

    units = Markup.escape . (' ':)

    value factor = Markup.escape (show (s `div` factor))

newSizeView :: Maybe Pixbuf
            -> Maybe Pixbuf
            -> IO (ListStore SizeInfo, TreeView)
newSizeView methodIcon_ signalIcon_ = do
  sizeStore <- listStoreNew []
  sizeView <- treeViewNewWithModel sizeStore

  set sizeView [ treeViewHeadersVisible := True ]

  nameColumn <- treeViewColumnNew
  treeViewColumnSetTitle nameColumn "Member"
  set nameColumn [ treeViewColumnResizable := True
                 , treeViewColumnExpand := True
                 ]

  maybeAddTypeIconColumn nameColumn sizeStore methodIcon_ signalIcon_ $ \si ->
      case siType si of
          SizeSignal -> False
          -- We distinguish between call, return and error by <i> and red.
          _          -> True
  addMemberRenderer nameColumn sizeStore True formatSizeInfoMember
  treeViewAppendColumn sizeView nameColumn

  addStatColumn sizeView sizeStore "Smallest" (formatSize . siMinSize)
  addStatColumn sizeView sizeStore "Mean" (formatSize . siMeanSize)
  addStatColumn sizeView sizeStore "Largest" (formatSize . siMaxSize)

  return (sizeStore, sizeView)
