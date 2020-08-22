module ImageAnsiTerminalUtilsTest where

import Utils.HttpUtils (getImage)
import Utils.ImageAnsiTerminalUtils (imageToAnsi)
import Data.Maybe (fromJust)
import Codec.Picture.Extra (scaleBilinear)

printCover :: IO ()
printCover = do
    (status, mImage) <- getImage "https://i.scdn.co/image/ab67616d00004851cda9b715df60a7d8fe0a9d62"
    let image = fromJust mImage
    let scaled = scaleBilinear 30 15 image
    putStrLn $ imageToAnsi scaled