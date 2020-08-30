module Controller (play, search) where
    
import AppState (AppStateIO, isPlaying, _showSearch, _searchInput, _trackName)
import Control.Lens (assign)

import           AppState     (AppState (AppState), AppStateIO, isPlaying,
                               _accessToken, _isPlaying)
import           Control.Lens (assign)

search :: String -> AppStateIO ()
search s = undefined

initAppState :: IO AppState
initAppState = return AppState {
        _accessToken = Nothing,
        _isPlaying = False,
        _showSearch = True,
        _searchInput = Nothing,
        _trackName = Nothing
    }

play :: AppStateIO ()
play = do assign isPlaying True
