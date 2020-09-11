-- |
--  Author: Benedikt Spies
--
--  Brick Widget to right-align Widget
module Widgets.Right where

import Brick (Padding (Pad), Size (Greedy), Widget (Widget, render), availWidthL, getContext, imageL, raw)
import Brick.Types (Widget (vSize))
import Brick.Widgets.Core (padLeft)
import Control.Lens ((^.))
import Graphics.Vty (imageWidth)

right :: Widget n -> Widget n
right p = Widget Greedy (vSize p) $ do
  result <- render p
  c <- getContext
  let image = result ^. imageL
  let width = imageWidth image
  let availableWidth = c ^. availWidthL
  let pad = Pad (availableWidth - width)
  render $ padLeft pad $ raw image
