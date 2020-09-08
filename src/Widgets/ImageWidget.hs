module Widgets.ImageWidget where

import Brick (Size (Greedy), Widget (render), availHeightL, availWidthL, getContext, raw)
import Brick.Types (Widget (Widget))
import Codec.Picture (Image, PixelRGB8)
import Codec.Picture.Extra (scaleBilinear)
import Control.Lens ((^.))
import Utils.ImageAnsiTerminalUtils (imageToVty, imageToVty240)

-- | Image height has to be even
imageWidget :: Image PixelRGB8 -> Widget n
imageWidget image = raw $ imageToVty image

-- | Image height has to be even
imageWidgetOfSize :: Int -> Int -> Image PixelRGB8 -> Widget n
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
      let width' = floorToEven $ c ^. availWidthL
      let height' = c ^. availHeightL * 2
      let size = min width' height'
      render $ imageWidgetOfSize size size image

-- | Only support 240 colors
imageWidget240 :: Image PixelRGB8 -> Widget n
imageWidget240 image = raw $ imageToVty240 image

-- | Image height has to be even
-- Only support 240 colors
imageWidget240OfSize :: Int -> Int -> Image PixelRGB8 -> Widget n
imageWidget240OfSize width height image = do
  let scaled = scaleBilinear width height image
  imageWidget240 scaled

-- | Only support 240 colors
greedyRectangularImageWidget240 :: Image PixelRGB8 -> Widget n
greedyRectangularImageWidget240 image = Widget width height render'
  where
    width = Greedy
    height = Greedy
    render' = do
      c <- getContext
      let width' = floorToEven $ c ^. availWidthL
      let height' = c ^. availHeightL * 2
      let size = min width' height'
      render $ imageWidget240OfSize size size image

-- helper

floorToEven :: Int -> Int
floorToEven x
  | (even x) = x
  | otherwise = x - 1