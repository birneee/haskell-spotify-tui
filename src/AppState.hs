{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module AppState where

import           ApiObjects.AccessToken    (AccessToken)
import           Control.Lens              (assign, makeLenses, modifying, use,
                                            view, (%=), (.=))
import           Control.Monad             (void)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Trans.State (StateT, evalStateT, execStateT, get)
import           Data.Functor.Identity     (Identity, runIdentity)

data AppState = AppState {
    _accessToken :: Maybe AccessToken,
    _isPlaying   :: Bool,
    _showSearch  :: Bool,
    _searchInput :: Maybe String,
    _trackName   :: Maybe String
} deriving(Show, Eq)

$(makeLenses ''AppState)

newAppState :: AppState
newAppState = AppState {
    _accessToken = Nothing,
    _isPlaying = False,
    _showSearch = False,
    _searchInput = Nothing,
    _trackName = Nothing
}

type AppStateT = StateT AppState
type AppStateM a = forall m. (Monad m) => AppStateT m a
type AppStateIO a = AppStateT IO a

evalAppState :: AppStateM a -> AppState -> a
evalAppState m = runIdentity . evalStateT m

execAppState :: AppStateM a -> AppState -> AppState
execAppState m = runIdentity . execStateT m

evalAppStateIO :: AppStateIO a -> AppState -> IO a
evalAppStateIO m = evalStateT m

execAppStateIO :: AppStateIO a -> AppState -> IO AppState
execAppStateIO m = execStateT m

-- example

togglePlay :: AppStateM ()
togglePlay = modifying isPlaying not

pause :: AppStateM ()
pause = assign isPlaying False

getIsPlaying :: AppStateM Bool
getIsPlaying = use isPlaying

-- test

execTest :: IO ()
execTest = void $ execAppStateIO test newAppState

test :: AppStateIO ()
test = do
    get >>= liftIO . print
    liftIO $ print "toggle play"
    togglePlay
    get >>= liftIO . print
    liftIO $ print "toggle play"
    togglePlay
    get >>= liftIO . print
