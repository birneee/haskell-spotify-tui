module Controller (Controller.play, search, initAppState) where

import ApiClient as API (next, pause, play, searchTrack)
import ApiObjects.AccessToken (AccessToken)
import ApiObjects.RefreshToken (RefreshToken)
-- import ApiObjects.SearchResponse   (Track)

import ApiObjects.Track (Track)
import AppState (AppState (AppState), AppStateIO, accessToken, isPlaying, searchInput, _accessToken, _isPlaying, _searchInput, _searchResults, _showSearch, _trackName)
import qualified Authenticator as A
  ( getAccessToken,
    getAuthorizationCode,
    getRefreshToken,
  )
import Control.Lens (assign, use, view, (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Persistence as P
  ( loadRefreshToken,
    saveRefreshToken,
  )
import Utils.ImageGenerator (generateRainbowImage)
import Utils.StatusLenses (code)

search :: AppStateIO ()
search = do
  input <- use searchInput
  liftIO $ putStrLn input
  at <- use accessToken
  (status, response) <- liftIO $ searchTrack at input
  --  case status of
  return ()

initAppState :: IO AppState
initAppState =
  return
    AppState
      { _accessToken = undefined,
        _isPlaying = False,
        _showSearch = True,
        _searchInput = "",
        _trackName = Nothing,
        _searchResults = []
      }

-- togglePlay :: AppStateIO ()
-- togglePlay = do assign isPlaying True

play :: AppStateIO ()
play = do
  at <- use accessToken
  status <- liftIO $ API.play at
  case (status ^. code) of
    202 -> assign isPlaying True
    204 -> assign isPlaying True
    _ -> return () -- TODO handle error

pause :: AppStateIO ()
pause = do
  at <- use accessToken
  status <- liftIO $ API.pause at
  case (status ^. code) of
    202 -> assign isPlaying False
    204 -> assign isPlaying False
    _ -> return () -- TODO handle error

next :: AppStateIO ()
next = do
  at <- use accessToken
  status <- liftIO $ API.next at
  case (status ^. code) of
    202 -> assign isPlaying True
    204 -> assign isPlaying True
    _ -> return () -- TODO handle error

previous :: AppStateIO ()
previous = do
  at <- use accessToken
  status <- liftIO $ API.pause at
  case (status ^. code) of
    202 -> assign isPlaying True
    204 -> assign isPlaying True
    _ -> return () -- TODO handle error

requestAccessToken :: IO AccessToken

-- | Request new access token on Spotify API
requestAccessToken = loadRefreshToken >>= A.getAccessToken

loadRefreshToken :: IO RefreshToken

-- | Load refresh token from file or request a new one from the Spotify API
loadRefreshToken = do
  rt <- P.loadRefreshToken
  case rt of
    Just rt -> return rt
    Nothing -> do
      rt <- A.getAuthorizationCode >>= A.getRefreshToken
      P.saveRefreshToken rt
      return rt
