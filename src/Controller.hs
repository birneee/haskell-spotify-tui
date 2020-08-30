module Controller (play, pause, search) where
    
import AppState (AppStateIO, isPlaying)
import Control.Lens (assign)

play :: AppStateIO ()
play = do assign isPlaying True

pause :: AppStateIO ()
pause = do assign isPlaying False

search :: String -> AppStateIO ()
search s = undefined


