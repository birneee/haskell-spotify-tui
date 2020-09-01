-- |TODO refresh access token if expired
-- |TODO request new refresh token if not valid
module Controller (initAppState, play, search, requestAccessToken) where

import qualified ApiClient               as API (pause, play)
import           ApiObjects.AccessToken  (AccessToken)
import           ApiObjects.RefreshToken (RefreshToken)
import           AppState                (AppState (AppState), AppStateIO,
                                          accessToken, isPlaying, _accessToken,
                                          _albumCover, _albumName, _artistNames,
                                          _deviceId, _isPlaying, _searchInput,
                                          _showSearch, _trackName)
import qualified Authenticator           as A (getAccessToken,
                                               getAuthorizationCode,
                                               getRefreshToken)
import           Control.Lens            (assign, use, (^.))
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Data.Maybe              (fromJust)
import           GHC.Base                (Alternative ((<|>)))
import qualified Persistence             as P (loadRefreshToken,
                                               saveRefreshToken)
import           Utils.ImageGenerator    (generateRainbowImage)
import           Utils.StatusLenses      (code)

-- |placeholder that is displayed when no album cover is available
defaultAlbumCover = generateRainbowImage

search :: String -> AppStateIO ()
search s = undefined

initAppState :: IO AppState
-- | create a fresh AppState instance
-- | this is called on app startup
initAppState = do
    accessToken <- requestAccessToken
    return AppState {
        _accessToken = accessToken,
        _isPlaying = False,
        _deviceId = Nothing,
        _trackName = Nothing,
        _albumName = Nothing,
        _artistNames = [],
        _albumCover = defaultAlbumCover,
        _showSearch = False,
        _searchInput = ""
    }

play :: AppStateIO ()
play = do
    at <- use accessToken
    status <- liftIO $ API.play at
    case (status ^. code) of
        202 -> assign isPlaying True
        204 -> assign isPlaying True
        _   -> return () -- TODO handle error

pause :: AppStateIO ()
pause = do
    at <- use accessToken
    status <- liftIO $ API.pause at
    case (status ^. code) of
        202 -> assign isPlaying False
        204 -> assign isPlaying False
        _   -> return () -- TODO handle error

requestAccessToken :: IO AccessToken
-- |Request new access token on Spotify API
requestAccessToken = loadRefreshToken >>= A.getAccessToken

loadRefreshToken :: IO RefreshToken
-- |Load refresh token from file or request a new one from the Spotify API
loadRefreshToken = do
    rt <- P.loadRefreshToken
    case rt of
        Just rt -> return rt
        Nothing -> do
            rt <- A.getAuthorizationCode >>= A.getRefreshToken
            P.saveRefreshToken rt
            return rt
