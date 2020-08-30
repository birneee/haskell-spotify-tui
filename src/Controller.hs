module Controller where

import           AppState     (AppState (AppState), AppStateIO, isPlaying,
                               _accessToken, _isPlaying)
import           Control.Lens (assign)

initAppState :: IO AppState
initAppState = return AppState {
        _accessToken = Nothing,
        _isPlaying = False
    }

play :: AppStateIO ()
play = do assign isPlaying True
