module ImageGeneratorTest where

import           Codec.Picture        (DynamicImage (ImageRGB8))
import           Codec.Picture.Saving (imageToBitmap)
import           Data.ByteString.Lazy (hPut)
import           GHC.IO.Handle        (hFlush)
import           System.IO            (Handle)
import           System.IO.Temp       (withSystemTempFile)
import           Utils.ImageGenerator (generateMandelbrotImage)
import           Web.Browser          (openBrowser)

testMandelbrot :: IO ()
testMandelbrot = showImage $ ImageRGB8 $ generateMandelbrotImage 500 500

showImage :: DynamicImage -> IO ()
showImage image = withSystemTempFile ".bmp" writeAndShow
    where
        writeAndShow :: FilePath -> Handle -> IO ()
        writeAndShow filePath handle = do
            let bytes = imageToBitmap image
            hPut handle bytes
            hFlush handle
            openBrowser $ "file://" ++ filePath
            getChar
            return ()
