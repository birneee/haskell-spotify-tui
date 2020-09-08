module Widgets.Right where

import Brick (Widget)
import Brick.Types (Widget(vSize))
import Brick (Size(Greedy))
import Brick (Widget(Widget))
import Brick (Widget(render))
import Brick (getContext)
import Brick (imageL)
import Control.Lens ((^.))
import Graphics.Vty (imageWidth)
import Brick (availWidthL)
import Brick.Widgets.Core (padLeft)
import Brick (raw)
import Brick (Padding(Pad))

right :: Widget n -> Widget n
right p = Widget Greedy (vSize p) $ do
    result <- render p
    c <- getContext
    let image = result ^. imageL
    let width = imageWidth image
    let availableWidth = c ^. availWidthL
    let pad = Pad (availableWidth - width)
    render $ padLeft pad $ raw image
