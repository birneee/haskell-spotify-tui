module Controller (play, search, initAppState) where
    
import AppState (AppStateIO, isPlaying, _showSearch, _searchInput, _trackName)
import Control.Lens (assign)

import           AppState             (AppState (AppState), AppStateIO,
                                       isPlaying, _accessToken, _albumCover,
                                       _isPlaying)
import           Control.Lens         (assign)
import           Utils.ImageGenerator (generateRainbowImage)

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
