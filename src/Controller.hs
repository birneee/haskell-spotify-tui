module Controller where
    
import AppState (AppStateIO, isPlaying)
import Control.Lens (assign)

play :: AppStateIO ()
play = do assign isPlaying True
