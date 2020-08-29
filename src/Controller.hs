module Controller (play, pause) where
    
import AppState (AppStateIO, isPlaying)
import Control.Lens (assign)

play :: AppStateIO ()
play = do assign isPlaying True

pause :: AppStateIO ()
pause = do assign isPlaying False


