{-# LANGUAGE OverloadedStrings #-}

module Utils.ImageAnsiTerminalUtils (imageToAnsi, imageToVty240, imageToVty) where

import Codec.Picture (Image (imageHeight, imageWidth), PixelRGB8 (PixelRGB8), pixelAt)
import Data.List (intercalate)
import Data.String.AnsiEscapeCodes.Strip.Text.Lazy (stripAnsiEscapeCodes)
import Data.Text.Lazy (pack, unpack)
import qualified Graphics.Vty as VTY
  ( Color,
    char,
    defAttr,
    horizCat,
    rgbColor,
    safeWcswidth,
    vertCat,
    withBackColor,
    withForeColor,
  )
import qualified Graphics.Vty.Image.Internal as VTY (Image (HorizText))
import Text.Printf (printf)

-- | see https://www.fileformat.info/info/unicode/char/2580/index.htm
upperHalfBlock :: [Char]
upperHalfBlock = "\x2580"

imageDoubleRow :: Image PixelRGB8 -> Int -> [(PixelRGB8, PixelRGB8)]
imageDoubleRow image y = zip (imageRow image y) (imageRow image (y + 1))

imageRow :: Image PixelRGB8 -> Int -> [PixelRGB8]
imageRow image y = pixelAtX <$> [0 .. (width -1)]
  where
    pixelAtX x = pixelAt image x y
    width = imageWidth image

foregroundColorEscapeSequence :: PixelRGB8 -> String
foregroundColorEscapeSequence (PixelRGB8 r g b) = printf "\ESC[38;2;%d;%d;%dm" r g b

backgroundColorEscapeSequence :: PixelRGB8 -> String
backgroundColorEscapeSequence (PixelRGB8 r g b) = printf "\ESC[48;2;%d;%d;%dm" r g b

-- ansi

-- | Inspired by https://github.com/stefanhaustein/TerminalImageViewer
imageToAnsi :: Image PixelRGB8 -> String
imageToAnsi image = unlines $ rowAtY <$> [0, 2 .. (height -1)]
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

-- | This function is loosly, hackage Graphics.Vty only support 240 colors
imageToVty240 :: Image PixelRGB8 -> VTY.Image
imageToVty240 image = VTY.vertCat $ rowAtY <$> [0, 2 .. (height -1)]
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

-- | This function uses an ugly workaround to display all 256^3 colors
-- hackage Graphics.Vty only support 240 colors
imageToVty :: Image PixelRGB8 -> VTY.Image
imageToVty image = VTY.vertCat $ stringWithEscapes <$> lines (imageToAnsi image)

stringWithEscapes :: String -> VTY.Image
stringWithEscapes str = VTY.HorizText VTY.defAttr (pack modStr) outputWidth charWidth
  where
    outputWidth = VTY.safeWcswidth $ stripEscapes str
    charWidth = outputWidth
    modStr = workaround ++ str
    stripEscapes = unpack . stripAnsiEscapeCodes . pack
    workaround = repeatN "\ESC\ESCm" $ (length str - outputWidth)
    repeatN :: String -> Int -> String
    repeatN str' n = intercalate "" $ take n $ repeat str'
