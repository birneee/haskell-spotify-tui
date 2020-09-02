module Controller (play, search, initAppState, mandelbrot) where

import           AppState             (AppStateIO, albumCover, isPlaying,
                                       _searchInput, _showSearch, _trackName)
import           Control.Lens         (assign, (.=))

import           AppState             (AppState (AppState), AppStateIO,
                                       isPlaying, _accessToken, _albumCover,
                                       _isPlaying)
import           Control.Lens         (assign)
import           Control.Lens         (use)
import           Utils.ImageGenerator (generateMandelbrotImage,
                                       generateRainbowImage)

-- |placeholder that is displayed when no album cover is available
defaultAlbumCover = generateRainbowImage

search :: String -> AppStateIO ()
search s = undefined

initAppState :: IO AppState
initAppState = return AppState {
        _accessToken = Nothing,
        _isPlaying = False,
        _showSearch = True,
        _searchInput = Nothing,
        _trackName = Nothing,
        _albumCover = defaultAlbumCover
    }

play :: AppStateIO ()
play = do assign isPlaying True

mandelbrot :: AppStateIO ()
mandelbrot = do
    ac <- use albumCover
    if ac == defaultAlbumCover
        then albumCover .= generateMandelbrotImage 255 255
        else albumCover .= defaultAlbumCover
