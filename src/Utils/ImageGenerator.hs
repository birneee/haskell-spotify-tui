module Utils.ImageGenerator where

import Codec.Picture (Image, PixelRGB8 (PixelRGB8), generateImage)

generateRainbowImage :: Image PixelRGB8
generateRainbowImage = generateImage f 256 256
  where
    f :: Int -> Int -> PixelRGB8
    f x y = PixelRGB8 r g b
      where
        r = fromIntegral $ 255 - x
        g = fromIntegral $ 255 - y
        b = fromIntegral x
