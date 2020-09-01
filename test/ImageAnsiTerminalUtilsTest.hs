module ImageAnsiTerminalUtilsTest where

import           Codec.Picture.Extra          (scaleBilinear)
import           Data.Maybe                   (fromJust)
import qualified Graphics.Vty                 as VTY
import           Utils.HttpUtils              (getImage)
import           Utils.ImageAnsiTerminalUtils (imageToAnsi, imageToVty)
import           Utils.ImageGenerator         (generateRainbowImage)
import Codec.Picture (Image, PixelRGB8)

albumCoverUrl = "https://i.scdn.co/image/ab67616d00004851cda9b715df60a7d8fe0a9d62"

printCover :: IO ()
printCover = do
    (status, mImage) <- getImage albumCoverUrl
    let image = fromJust mImage
    let scaled = scaleBilinear 30 30 image
    putStrLn $ imageToAnsi scaled

printCoverVty :: IO ()
printCoverVty = do
    (status, mImage) <- getImage albumCoverUrl
    let image = fromJust mImage
    let scaled = scaleBilinear 30 30 image
    printVty scaled

printRainbow :: IO ()
printRainbow = do
    let image = generateRainbowImage
    let scaled = scaleBilinear 30 30 image
    putStrLn $ imageToAnsi scaled

printRainbowVty :: IO ()
printRainbowVty = do
    let image = generateRainbowImage
    let scaled = scaleBilinear 30 30 image
    printVty scaled


-- helper

printVty :: Image PixelRGB8 -> IO ()
printVty image = do
    let vtyImage = imageToVty image
    let vtyPic = VTY.picForImage vtyImage
    vtyCfg <- VTY.standardIOConfig
    vty <- VTY.mkVty vtyCfg
    VTY.update vty vtyPic
    _ <- VTY.nextEvent vty
    VTY.shutdown vty