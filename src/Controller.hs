module Controller (togglePlay, search, initAppState) where
    
import AppState (accessToken, searchInput, AppStateIO, isPlaying, _showSearch, _searchInput, _trackName)
import Control.Lens (view, use, assign)

import           AppState     (AppState (AppState), AppStateIO, isPlaying,
                               _accessToken, _isPlaying)
import           Control.Lens (assign)
import Control.Monad.IO.Class (MonadIO(liftIO))
import ApiClient (searchTrack)

search :: AppStateIO ()
search = do
         input <- use searchInput
         liftIO $ putStrLn input
         at <- use accessToken
         (status, response) <- liftIO $ searchTrack at input
        --  case status of
         return ()

initAppState :: IO AppState
initAppState = return AppState {
        _accessToken = undefined,
        _isPlaying = False,
        _showSearch = True,
        _searchInput = "",
        _trackName = Nothing
    }

togglePlay :: AppStateIO ()
togglePlay = do assign isPlaying True
