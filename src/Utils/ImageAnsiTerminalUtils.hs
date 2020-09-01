{-# LANGUAGE OverloadedStrings #-}

module Utils.ImageAnsiTerminalUtils (imageToAnsi, imageToVty240, imageToVty) where

import           Codec.Picture               (Image (imageHeight, imageWidth),
                                              PixelRGB8 (PixelRGB8), imageData)
import           Codec.Picture               (pixelAt)
import           Data.List                   (intercalate)
import           Data.Text.Lazy              (pack)
import qualified Graphics.Vty                as VTY
import qualified Graphics.Vty.Image.Internal as VTY
import           Text.Printf                 (printf)

upperHalfBlock :: [Char]
-- | see https://www.fileformat.info/info/unicode/char/2580/index.htm
upperHalfBlock = "\x2580"

imageDoubleRow :: Image PixelRGB8 -> Int -> [(PixelRGB8, PixelRGB8)]
imageDoubleRow image y = zip (imageRow image y) (imageRow image (y + 1))

imageRow :: Image PixelRGB8 -> Int -> [PixelRGB8]
imageRow image y = pixelAtX <$> [0..(width-1)]
    where
        pixelAtX x = pixelAt image x y
        width = imageWidth image

foregroundColorEscapeSequence :: PixelRGB8 -> String
foregroundColorEscapeSequence (PixelRGB8 r g b) = printf "\ESC[38;2;%d;%d;%dm" r g b

backgroundColorEscapeSequence :: PixelRGB8 -> String
backgroundColorEscapeSequence (PixelRGB8 r g b) = printf "\ESC[48;2;%d;%d;%dm" r g b

-- ansi

imageToAnsi :: Image PixelRGB8 -> String
-- |Inspired by https://github.com/stefanhaustein/TerminalImageViewer
imageToAnsi image = unlines $ rowAtY <$> [0,2..(height-1)]
    where
        height
            | (even height') = height'
            | otherwise = error ("Height has to be even")
            where
                height' = imageHeight image
        rowAtY :: Int -> String
        rowAtY i = doublePixelRowToAnsi $ imageDoubleRow image i

doublePixelRowToAnsi :: [(PixelRGB8, PixelRGB8)] -> String
doublePixelRowToAnsi dps = intercalate "" $ doublePixelToAnsi <$> dps

doublePixelToAnsi :: (PixelRGB8, PixelRGB8) -> String
doublePixelToAnsi (top, bottom) = prefix ++ upperHalfBlock ++ postfix
    where
        prefix = foregroundPrefix ++ backgroundPrefix
        foregroundPrefix = foregroundColorEscapeSequence top
        backgroundPrefix = backgroundColorEscapeSequence bottom
        postfix = "\ESC[0m"

-- vty 240

imageToVty240 :: Image PixelRGB8 -> VTY.Image
-- |This function is loosly, hackage Graphics.Vty only support 240 colors
imageToVty240 image = VTY.vertCat $ rowAtY <$> [0,2..(height-1)]
    where
        height
            | (even height') = height'
            | otherwise = error ("Height has to be even")
            where
                height' = imageHeight image
        rowAtY :: Int -> VTY.Image
        rowAtY i = doublePixelRowToVty240 $ imageDoubleRow image i

doublePixelRowToVty240 :: [(PixelRGB8, PixelRGB8)] -> VTY.Image
doublePixelRowToVty240 dps = VTY.horizCat $ doublePixelToVty240 <$> dps

doublePixelToVty240 :: (PixelRGB8, PixelRGB8) -> VTY.Image
doublePixelToVty240 (top, bottom) = VTY.char attr '\x2580'
    where
        attr = VTY.defAttr `VTY.withForeColor` topColor `VTY.withBackColor` bottomColor
        topColor = pixelToVtyColor240 top
        bottomColor = pixelToVtyColor240 bottom

pixelToVtyColor240 :: PixelRGB8 -> VTY.Color
pixelToVtyColor240 (PixelRGB8 r g b) = VTY.rgbColor r g b

-- vty

imageToVty :: Image PixelRGB8 -> VTY.Image
-- |This function uses an ugly workaround to display all 256^3 colors
-- |hackage Graphics.Vty only support 240 colors
imageToVty image = VTY.vertCat $ rowAtY <$> [0,2..(height-1)]
    where
        height
            | (even height') = height'
            | otherwise = error ("Height has to be even")
            where
                height' = imageHeight image
        rowAtY :: Int -> VTY.Image
        rowAtY i = doublePixelRowToVty $ imageDoubleRow image i

doublePixelRowToVty :: [(PixelRGB8, PixelRGB8)] -> VTY.Image
doublePixelRowToVty dps = VTY.horizCat $ doublePixelToVty <$> dps

doublePixelToVty :: (PixelRGB8, PixelRGB8) -> VTY.Image
doublePixelToVty (top, bottom) = VTY.horizCat [foregroundPrefix, backgroundPrefix, content, postfix]
    where
        foregroundPrefix = zeroWidthStr $ foregroundColorEscapeSequence top
        backgroundPrefix = zeroWidthStr $ backgroundColorEscapeSequence bottom
        content = VTY.char VTY.defAttr '\x2580'
        postfix = zeroWidthStr "\ESC[0m"

zeroWidthStr :: String -> VTY.Image
-- | workaround to print any string in terminal, and hackage vty handles it as if it has width 0
zeroWidthStr str = VTY.HorizText VTY.defAttr (pack modStr) 0 0
    where
        modStr = str ++ repeatN "\ESC\ESCa" (length str)
        repeatN :: String -> Int -> String
        repeatN str n = intercalate "" $ take n $ repeat str
