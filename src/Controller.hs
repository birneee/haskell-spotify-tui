-- | TODO refresh access token if expired
--  |TODO request new refresh token if not valid
module Controller (initAppState, play, pause, next, previous, search, requestAccessToken, playSelectedTrack) where

import qualified ApiClient as API (next, pause, play, playTrack, previous, searchTrack)
import ApiObjects.AccessToken (AccessToken)
import ApiObjects.RefreshToken (RefreshToken)
import ApiObjects.SearchResponse (SearchResponse (SearchResponse), items)
import ApiObjects.Track (uri)
import AppState
  ( AppState (AppState),
    AppStateIO,
    accessToken,
    albumCover,
    isPlaying,
    searchInput,
    searchResults,
    selectedSearchResultIndex,
    _accessToken,
    _albumCover,
    _albumName,
    _artistNames,
    _deviceId,
    _isPlaying,
    _searchInput,
    _searchResults,
    _selectedSearchResultIndex,
    _showSearch,
    _trackName,
  )
import qualified Authenticator as A
  ( getAccessToken,
    getAuthorizationCode,
    getRefreshToken,
  )
import Control.Lens (assign, ix, preuse, preview, previews, use, (.=), (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromJust)
import GHC.Base (Alternative ((<|>)))
import qualified Persistence as P
  ( loadRefreshToken,
    saveRefreshToken,
  )
import Utils.ImageGenerator (generateMandelbrotImage, generateRainbowImage)
import Utils.StatusLenses (code)

-- | placeholder that is displayed when no album cover is available
defaultAlbumCover = generateRainbowImage

search :: AppStateIO ()
search = do
  input <- use searchInput
  at <- use accessToken
  (status, response) <- liftIO $ API.searchTrack at input
  case (status ^. code, response) of
    (200, Just (SearchResponse (Just tracks))) -> searchResults .= tracks ^. items
    _ -> return ()

initAppState :: IO AppState

-- | create a fresh AppState instance
-- | this is called on app startup
initAppState = do
  accessToken <- requestAccessToken
  return
    AppState
      { _accessToken = accessToken,
        _isPlaying = False,
        _deviceId = Nothing,
        _trackName = Nothing,
        _albumName = Nothing,
        _artistNames = [],
        _albumCover = defaultAlbumCover,
        _showSearch = True,
        _searchInput = "",
        _searchResults = [],
        _selectedSearchResultIndex = 0
      }

play :: AppStateIO ()
play = do
  liftIO $ putStrLn "Test"
  at <- use accessToken
  status <- liftIO $ API.play at
  -- liftIO $ putStrLn $ show status
  case (status ^. code) of
    202 -> assign isPlaying True
    204 -> assign isPlaying True
    _ -> return () -- TODO handle error

playSelectedTrack :: AppStateIO ()
playSelectedTrack = do
  index <- use selectedSearchResultIndex
  uri <- preuse (searchResults . ix index . uri)
  at <- use accessToken
  status <- liftIO $ API.playTrack at (fromJust uri)
  -- liftIO $ putStrLn $ show status
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
    202 -> assign isPlaying True --TODO check Status Code
    204 -> assign isPlaying True --TODO check Status Code
    _ -> return () -- TODO handle error

previous :: AppStateIO ()
previous = do
  at <- use accessToken
  status <- liftIO $ API.previous at
  case (status ^. code) of
    202 -> assign isPlaying True --TODO check Status Code
    204 -> assign isPlaying True --TODO check Status Code
    _ -> return () -- TODO handle error

mandelbrot :: AppStateIO ()
mandelbrot = do
  ac <- use albumCover
  if ac == defaultAlbumCover
    then albumCover .= generateMandelbrotImage 256 256
    else albumCover .= defaultAlbumCover

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
