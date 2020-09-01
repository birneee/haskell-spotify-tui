module Widgets.ImageWidget where

import Brick (raw)
import Utils.ImageAnsiTerminalUtils (imageToVty240, imageToVty)
import Brick.Types (Widget(Widget))
import Codec.Picture (Image, PixelRGB8)
import Brick (Size(Greedy))
import Brick (Widget(render))
import Brick (getContext)
import Control.Lens ((^.))
import Brick (availWidthL)
import Brick (availHeightL)
import Codec.Picture.Extra (scaleBilinear)

imageWidget :: Image PixelRGB8 -> Widget n
-- |Image height has to be even
imageWidget image = raw $ imageToVty image

imageWidgetOfSize :: Int -> Int -> Image PixelRGB8 -> Widget n
-- |Image height has to be even
imageWidgetOfSize width height image = do
    let scaled = scaleBilinear width height image
    imageWidget scaled

greedyRectangularImageWidget :: Image PixelRGB8 -> Widget n
greedyRectangularImageWidget image = Widget width height render'
    where 
        width = Greedy
        height = Greedy
        render' = do
            c <- getContext
            let width = floorToEven $ c ^. availWidthL
            let height = c ^. availHeightL * 2
            let size = min width height
            render $ imageWidgetOfSize size size image

imageWidget240 :: Image PixelRGB8 -> Widget n
imageWidget240 image = raw $ imageToVty240 image

-- helper

floorToEven :: Int -> Int
floorToEven x 
    | (even x) = x
    | otherwise = x - 1