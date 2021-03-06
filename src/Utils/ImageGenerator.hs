{-# LANGUAGE RankNTypes #-}

-- |
--  Author: Benedikt Spies
--
--  Utility functions to generate simple Images
--  Image rows are generated in parallel
module Utils.ImageGenerator (generateRainbowImage, generateMandelbrotImage) where

import Codec.Picture
  ( Image,
    Pixel8,
    PixelRGB8 (PixelRGB8),
  )
import Data.Complex (Complex ((:+)), magnitude)
import Utils.ParallelImageUtils (parGenerateImage)

generateRainbowImage :: Image PixelRGB8
generateRainbowImage = parGenerateImage f 256 256
  where
    f :: Int -> Int -> PixelRGB8
    f x y = PixelRGB8 r g b
      where
        r = fromIntegral $ 255 - x
        g = fromIntegral $ 255 - y
        b = fromIntegral x

generateMandelbrotImage :: Int -> Int -> Image PixelRGB8
generateMandelbrotImage width height = parGenerateImage (generateMandelbrotPixel width height) width height

generateMandelbrotPixel :: Int -> Int -> Int -> Int -> PixelRGB8
generateMandelbrotPixel imageWidth imageHeight x y = color $ mandelbrot (x' / w' * 3 - 2) (y' / h' * 3 - 1.5)
  where
    x' = fromIntegral x
    y' = fromIntegral y
    w' = fromIntegral imageWidth
    h' = fromIntegral imageHeight
    maxIter = 300
    color :: Int -> PixelRGB8
    color iter = PixelRGB8 colorVal colorVal colorVal
      where
        colorVal = floor $ 255 - amt * 255 :: Pixel8
        amt = fromIntegral iter / fromIntegral maxIter :: Float
    mandelbrot :: Float -> Float -> Int
    mandelbrot r i =
      length . takeWhile (\z -> magnitude z <= 2)
        . take maxIter
        $ iterate (\z -> z ^ (2 :: Int) + (r :+ i)) 0
