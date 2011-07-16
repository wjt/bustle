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
import Graphics.UI.Gtk.Glade
import Bustle.Stats
import Bustle.Types (Log)
import qualified Bustle.Markup as Markup
import Bustle.Markup (Markup)

data StatsPane =
    StatsPane { spCountStore :: ListStore FrequencyInfo
              , spTimeStore :: ListStore TimeInfo
              }

statsPaneNew :: GladeXML
             -> Maybe Pixbuf
             -> Maybe Pixbuf
             -> IO StatsPane
statsPaneNew xml methodIcon signalIcon = do
  [frequencySW, durationSW] <- mapM (xmlGetWidget xml castToScrolledWindow)
      ["frequencySW", "durationSW"]

  (countStore, countView) <- newCountView methodIcon signalIcon
  containerAdd frequencySW countView

  (timeStore, timeView) <- newTimeView
  containerAdd durationSW timeView

  return $ StatsPane countStore timeStore

statsPaneSetMessages :: StatsPane
                     -> Log -- ^ session bus messages
                     -> Log -- ^ system bus messages
                     -> IO ()
statsPaneSetMessages sp sessionMessages systemMessages = do
    -- This conflates messages on the system bus and on the session bus,
    -- but I think that's okay for now.
    let allMessages = sessionMessages ++ systemMessages

    forM_ (frequencies allMessages) $ listStoreAppend (spCountStore sp)
    forM_ (methodTimes allMessages) $ listStoreAppend (spTimeStore sp)

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

  -- If we managed to load the method and signal icons...
  case (method, signal) of
      (Just m, Just s) -> do
          typeRenderer <- cellRendererPixbufNew
          cellLayoutPackStart nameColumn typeRenderer False
          cellLayoutSetAttributes nameColumn typeRenderer countStore $
              \fi ->
                  [ cellPixbuf := case fiType fi of
                                      TallyMethod -> m
                                      TallySignal -> s
                  ]
      _ -> return ()

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
