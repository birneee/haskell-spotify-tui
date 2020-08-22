module Utils.ImageAnsiTerminalUtils where

import           Codec.Picture (pixelAt)
import           Codec.Picture (Image (imageHeight, imageWidth),
                                PixelRGB8 (PixelRGB8))
import           Data.List     (intercalate)

reset :: String
reset = "\ESC[0m"

imageToAnsi :: Image PixelRGB8 -> String
-- |Inspired by https://github.com/stefanhaustein/TerminalImageViewer
imageToAnsi image = do
    let height = imageHeight image
    intercalate "\n" $ (\y -> rowToAnsi image y) <$> [0..(height-1)]
    where
        rowToAnsi :: Image PixelRGB8 -> Int -> String
        rowToAnsi image row = do
            let width = imageWidth image
            intercalate "" $ (\x -> pixelToAnsi $ pixelAt image x row) <$> [0..(width-1)]

pixelToAnsi :: PixelRGB8 -> String
pixelToAnsi (PixelRGB8 r g b) = "\ESC[48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m" ++ " " ++ reset
