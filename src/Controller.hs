module Controller (play, pause, search) where
    
import AppState (AppStateIO, isPlaying)
import Control.Lens (assign)

import           AppState     (AppState (AppState), AppStateIO, isPlaying,
                               _accessToken, _isPlaying)
import           Control.Lens (assign)

search :: String -> AppStateIO ()
search s = undefined

initAppState :: IO AppState
initAppState = return AppState {
        _accessToken = Nothing,
        _isPlaying = False
    }

play :: AppStateIO ()
play = do assign isPlaying True
