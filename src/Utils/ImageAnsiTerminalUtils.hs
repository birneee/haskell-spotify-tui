module Utils.ImageAnsiTerminalUtils (imageToAnsi) where

import           Codec.Picture (Image (imageHeight, imageWidth),
                                PixelRGB8 (PixelRGB8))
import           Codec.Picture (pixelAt)
import           Data.List     (intercalate)

foregroundEscapePrefix :: [Char]
foregroundEscapePrefix = "\ESC[38;2;"

backgroundEscapePrefix :: [Char]
backgroundEscapePrefix = "\ESC[48;2;"

reset :: String
reset = "\ESC[0m"

upperHalfBlock :: [Char]
upperHalfBlock = "\x2580"

imageToAnsi :: Image PixelRGB8 -> String
-- |Inspired by https://github.com/stefanhaustein/TerminalImageViewer
imageToAnsi image = do
    let height = imageHeight image
    intercalate "\n" $ (\y -> buildDoubleRow image y) <$> [0,2..(height-1)]

buildDoubleRow :: Image PixelRGB8 -> Int -> String
buildDoubleRow image row = do
    let width = imageWidth image
    let rows = (\x -> doublePixelAtToAnsi image x row) <$> [0..(width-1)]
    (intercalate "" $ rows) ++ reset

doublePixelAtToAnsi :: Image PixelRGB8 -> Int -> Int -> String
doublePixelAtToAnsi image x y = do
    let upperPixel = pixelAt image x y
    let lowerPixel = pixelAt image x (y+1)
    let (PixelRGB8 r g b) = upperPixel
    let foregroundEscape = foregroundEscapePrefix ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"
    let (PixelRGB8 r g b) = lowerPixel
    let backgroundEscape = backgroundEscapePrefix ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"
    foregroundEscape ++ backgroundEscape ++ upperHalfBlock
