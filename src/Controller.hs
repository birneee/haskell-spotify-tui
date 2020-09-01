module Controller (initAppState, play) where

import           AppState             (AppState (AppState), AppStateIO,
                                       isPlaying, _accessToken, _albumCover,
                                       _isPlaying)
import           Control.Lens         (assign)
import           Utils.ImageGenerator (generateRainbowImage)

-- |placeholder that is displayed when no album cover is available
defaultAlbumCover = generateRainbowImage

initAppState :: IO AppState
initAppState = return AppState {
        _accessToken = Nothing,
        _isPlaying = False,
        _albumCover = defaultAlbumCover
    }

play :: AppStateIO ()
play = do assign isPlaying True
