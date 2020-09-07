{-# LANGUAGE RankNTypes #-}

module Utils.ImageUtils where

import Codec.Picture
  ( Image (Image),
    Pixel,
    PixelRGB8,
    generateImage,
    imageData,
    imageHeight,
    imageWidth,
  )
import Control.Monad.Par (NFData, Par, get, runPar, spawn)
import qualified Data.Vector.Storable as V

parGenerateImage :: (Int -> Int -> PixelRGB8) -> Int -> Int -> Image PixelRGB8
parGenerateImage f width height = do
  unsafeConcatRows $ runPar $ parMap generateRow [0 .. height -1]
  where
    generateRow :: Int -> Image PixelRGB8
    generateRow y = generateImage (\x _ -> f x y) width 1

unsafeConcatRows :: forall px. Pixel px => [Image px] -> Image px
unsafeConcatRows rows = do
  let width = imageWidth $ head rows
  let height = length rows * (imageHeight $ head rows)
  let d = V.concat $ imageData <$> rows
  Image width height d

parMap :: NFData b => (a -> b) -> [a] -> Par [b]
parMap f xs = do
  ibs <- mapM (spawn . return . f) xs
  mapM get ibs
