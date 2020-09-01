module AppStateTest where

import           AppState                  (AppState (AppState), AppStateIO,
                                            AppStateM, execAppStateIO,
                                            isPlaying, _accessToken, _isPlaying)
import           Control.Lens              (assign, modifying, use, (%=), (.=),
                                            (^.))
import           Control.Monad             (void)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Trans.State (get)

emptyAppState :: AppState
emptyAppState = AppState {
    _isPlaying = False
}

togglePlay :: AppStateM ()
togglePlay = isPlaying %= not

pause :: AppStateM ()
pause = isPlaying .= False

play :: AppStateM ()
play = isPlaying .= True

getIsPlaying :: AppStateM Bool
getIsPlaying = use isPlaying

execTest :: IO ()
execTest = void $ execAppStateIO test emptyAppState

test :: AppStateIO ()
test = do
    get >>= liftIO . print
    liftIO $ print "toggle play"
    togglePlay
    get >>= liftIO . print
    liftIO $ print "toggle play"
    togglePlay
    get >>= liftIO . print
