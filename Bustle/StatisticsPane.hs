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
  ( newCountView
  , newTimeView
  )
where

import Text.Printf

import Control.Applicative ((<$>))

import Bustle.Stats

import Graphics.UI.Gtk

addTextRenderer :: TreeViewColumn
                -> ListStore a
                -> Bool
                -> (a -> String)
                -> IO CellRendererText
addTextRenderer col store expand f = do
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer expand
    set renderer [ cellTextSizePoints := 7 ]
    cellLayoutSetAttributes col renderer store $ \x ->
        [ cellTextMarkup := Just $ f x ]
    return renderer

addMemberRenderer :: TreeViewColumn
                  -> ListStore a
                  -> Bool
                  -> (a -> String)
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
              -> (a -> String)
              -> IO ()
addStatColumn view store title f = do
    col <- treeViewColumnNew
    treeViewColumnSetTitle col title
    renderer <- addTextRenderer col store True f
    set renderer [ cellXAlign := 1 ]
    treeViewAppendColumn view col
    return ()

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
      fiInterface fi ++ "<b>" ++ fiMember fi ++ "</b>"
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
      tiInterface ti ++ "<b>" ++ tiMethodName ti ++ "</b>"
  treeViewAppendColumn timeView nameColumn

  addStatColumn timeView timeStore "Total time"
                (printf "%.3f" . tiTotalTime)
  addStatColumn timeView timeStore "Calls" (show . tiCallFrequency)
  addStatColumn timeView timeStore "Mean time"
                (printf "%.3f" . tiMeanCallTime)

  return (timeStore, timeView)

