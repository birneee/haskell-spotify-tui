{-# LANGUAGE MultiWayIf #-}

-- |
--  Author: Benedikt Spies, Kai-Chun Lin
--
--  Controlling the application and modifying the AppState
--  The controller uses ApiClient to access Spotify API
--
--  TODO refresh access token if expired
--  TODO request new refresh token if not valid
module Controller where

import qualified ApiClient as API (getAvailableDevices, getCurrentAlbumCover, getPlayer, next, pause, play, playTrack, previous, searchTrack, setPlayer, setVolume)
import ApiObjects.AccessToken (AccessToken)
import qualified ApiObjects.Album as ALBUM (albumName, images)
import qualified ApiObjects.Artist as ARTIST (artistName)
import ApiObjects.Device (DeviceId)
import qualified ApiObjects.Device as DEVICE (deviceId, deviceName, deviceType, volumePercent)
import qualified ApiObjects.DevicesResponse as DR (devices)
import qualified ApiObjects.Image as IMAGE (url)
import qualified ApiObjects.PlayerResponse as PR (device, isPlaying, item, progressMs)
import ApiObjects.RefreshToken (RefreshToken)
import ApiObjects.SearchResponse (SearchResponse (SearchResponse), items)
import ApiObjects.Track (uri)
import qualified ApiObjects.Track as TRACK (album, artists, durationMs, popularity, trackName, uri)
import AppState
  ( AlbumCover,
    AppState (AppState),
    AppStateIO,
    accessToken,
    albumCover,
    albumCoverUrl,
    albumName,
    artistNames,
    deviceId,
    deviceName,
    deviceType,
    deviceVolumePercent,
    durationMs,
    isPlaying,
    latestLocalProgressUpdateTimestamp,
    packAlbumCover,
    progressMs,
    searchInput,
    searchResults,
    selectedSearchResultIndex,
    showSearch,
    trackName,
    trackPopularity,
    trackUri,
    _accessToken,
    _albumCover,
    _albumCoverUrl,
    _albumName,
    _artistNames,
    _deviceId,
    _deviceName,
    _deviceType,
    _deviceVolumePercent,
    _durationMs,
    _isPlaying,
    _latestLocalProgressUpdateTimestamp,
    _progressMs,
    _searchInput,
    _searchResults,
    _selectedSearchResultIndex,
    _showSearch,
    _trackName,
    _trackPopularity,
    _trackUri,
  )
import qualified Authenticator as A
  ( getAccessToken,
    getAuthorizationCode,
    getRefreshToken,
  )
import Control.Applicative (liftA)
import Control.Lens (ix, preuse, use, view, (%=), (.=), (^.), (^?), _Just)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types (statusIsSuccessful)
import qualified Persistence as P
  ( clientId,
    clientSecret,
    loadConfig,
    loadRefreshToken,
    saveRefreshToken,
  )
import Utils.ImageGenerator (generateMandelbrotImage, generateRainbowImage)
import Utils.MaybeUtils (forceMaybeMsg, (?:))
import Utils.StatusLenses (code)

-- | placeholder that is displayed when no album cover is available
defaultAlbumCover :: AlbumCover
defaultAlbumCover = packAlbumCover generateRainbowImage

-- | easter egg album cover
mandelbrotAlbumCover :: AlbumCover
mandelbrotAlbumCover = packAlbumCover $ generateMandelbrotImage 256 256

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
-- this is called on app startup
initAppState = do
  at <- requestAccessToken
  return
    AppState
      { _accessToken = at,
        _isPlaying = False,
        _deviceId = Nothing,
        _deviceName = Nothing,
        _deviceType = Nothing,
        _deviceVolumePercent = Nothing,
        _trackUri = Nothing,
        _trackName = Nothing,
        _trackPopularity = Nothing,
        _albumName = Nothing,
        _artistNames = [],
        _albumCoverUrl = Nothing,
        _albumCover = defaultAlbumCover,
        _showSearch = True,
        _searchInput = "",
        _searchResults = [],
        _selectedSearchResultIndex = 0,
        _progressMs = Nothing,
        _durationMs = Nothing,
        _latestLocalProgressUpdateTimestamp = Nothing
      }

play :: AppStateIO ()
play = do
  at <- use accessToken
  status <- liftIO $ API.play at
  -- liftIO $ putStrLn $ show status
  if
      | statusIsSuccessful status -> do
        isPlaying .= True
        updateCurrentTrackInfo
        return ()
      | otherwise -> return () -- TODO handle error

playSelectedTrack :: AppStateIO ()
playSelectedTrack = do
  index <- use selectedSearchResultIndex
  uri' <- preuse (searchResults . ix index . uri)
  at <- use accessToken
  status <- liftIO $ API.playTrack at (fromJust uri')
  -- liftIO $ putStrLn $ show status
  if
      | statusIsSuccessful status -> do
        isPlaying .= True
        showSearch .= False
        updateCurrentTrackInfo
        return ()
      | otherwise -> return () -- TODO handle error

pause :: AppStateIO ()
pause = do
  at <- use accessToken
  status <- liftIO $ API.pause at
  if
      | statusIsSuccessful status -> isPlaying .= False
      | otherwise -> return () -- TODO handle error

next :: AppStateIO ()
next = do
  at <- use accessToken
  status <- liftIO $ API.next at
  -- liftIO $ putStrLn $ show status
  if
      | statusIsSuccessful status -> do
        isPlaying .= True
        updateCurrentTrackInfo
        return ()
      | otherwise -> return () -- TODO handle error

previous :: AppStateIO ()
previous = do
  at <- use accessToken
  status <- liftIO $ API.previous at
  if
      | statusIsSuccessful status -> do
        isPlaying .= True
        updateCurrentTrackInfo
        return ()
      | otherwise -> return () -- TODO handle error

-- | download albumCoverUrl and set albumCover
updateAlbumCover :: AppStateIO ()
updateAlbumCover = do
  ac <- use albumCover
  if ac /= mandelbrotAlbumCover
    then downloadAlbumCover
    else return ()
  where
    downloadAlbumCover :: AppStateIO ()
    downloadAlbumCover = do
      at <- use accessToken
      (status, response) <- liftIO $ API.getCurrentAlbumCover at
      case (status ^. code, response) of
        (200, Just ac) -> albumCover .= packAlbumCover ac
        _ -> return () -- TODO handle error

updateCurrentTrackInfo :: AppStateIO ()
updateCurrentTrackInfo = do
  at <- use accessToken
  (status, response) <- liftIO $ API.getPlayer at
  case (status ^. code, response) of
    (200, Just pr) -> do
      isPlaying .= pr ^. (PR.isPlaying)
      deviceId .= Just (pr ^. (PR.device . DEVICE.deviceId))
      deviceName .= Just (pr ^. (PR.device . DEVICE.deviceName))
      deviceType .= Just (pr ^. (PR.device . DEVICE.deviceType))
      deviceVolumePercent .= Just (pr ^. (PR.device . DEVICE.volumePercent))
      trackUri .= pr ^? (PR.item . _Just . TRACK.uri)
      trackName .= pr ^? (PR.item . _Just . TRACK.trackName)
      trackPopularity .= pr ^? (PR.item . _Just . TRACK.popularity)
      albumName .= pr ^? (PR.item . _Just . TRACK.album . ALBUM.albumName)
      artistNames .= ((view ARTIST.artistName) <$> concat (pr ^? (PR.item . _Just . TRACK.artists)))
      albumCoverUrl .= pr ^? (PR.item . _Just . TRACK.album . ALBUM.images . ix 0 . IMAGE.url)
      durationMs .= pr ^? (PR.item . _Just . TRACK.durationMs)
      progressMs .= pr ^. (PR.progressMs)

      updateAlbumCover
    _ -> return () -- TODO handle error

-- | update track progress
updateProgress :: AppStateIO ()
updateProgress = do
  mOldTime <- use latestLocalProgressUpdateTimestamp
  isPlaying' <- use isPlaying
  now <- liftIO $ round <$> getPOSIXTime :: AppStateIO Int
  case (isPlaying', mOldTime) of
    (True, Just oldTime) -> do
      let diff = (now - oldTime) * 1000 -- s to ms
      progressMs %= liftA (+ diff)
      latestLocalProgressUpdateTimestamp .= Just now
    (True, Nothing) -> latestLocalProgressUpdateTimestamp .= Just now
    _ -> latestLocalProgressUpdateTimestamp .= Nothing

toggleDevice :: AppStateIO ()
toggleDevice = do
  at <- use accessToken
  (_, response) <- liftIO $ API.getAvailableDevices at
  case response of
    Nothing -> return () -- TODO handle error
    Just dr -> do
      mDid <- nextDeviceId
      case mDid of
        Nothing -> return () -- TODO handle error
        Just did -> do
          _ <- liftIO $ API.setPlayer at did -- TODO handle error
          updateCurrentTrackInfo
      where
        ids = (\d -> d ^. DEVICE.deviceId) <$> (dr ^. DR.devices)
        currentDeviceIndex :: AppStateIO Int
        currentDeviceIndex = do
          did' <- use deviceId
          return $ (elemIndex (did' ?: ("")) ids) ?: (-1)
        nextDeviceId :: AppStateIO (Maybe DeviceId)
        nextDeviceId = case ids of
          [] -> return Nothing
          _ -> do
            ci <- currentDeviceIndex
            return ((cycle ids) ^? (ix (ci + 1)))

-- | TODO handle error
volumeUp :: AppStateIO ()
volumeUp = do
  at <- use accessToken
  mCurrentVolume <- use deviceVolumePercent
  case mCurrentVolume of
    Nothing -> return ()
    Just volume -> do
      _ <- liftIO $ API.setVolume at (volume + 5)
      updateCurrentTrackInfo

-- | TODO handle error
volumeDown :: AppStateIO ()
volumeDown = do
  at <- use accessToken
  mCurrentVolume <- use deviceVolumePercent
  case mCurrentVolume of
    Nothing -> return ()
    Just volume -> do
      _ <- liftIO $ API.setVolume at (volume - 5)
      updateCurrentTrackInfo

mandelbrot :: AppStateIO ()
mandelbrot = do
  ac <- use albumCover
  if ac == mandelbrotAlbumCover
    then albumCover .= defaultAlbumCover
    else albumCover .= mandelbrotAlbumCover

-- | Request new access token on Spotify API
requestAccessToken :: IO AccessToken
requestAccessToken = do
  config <- P.loadConfig
  let clientId = forceMaybeMsg clientIdErrorMsg $ config ^. P.clientId
  let clientSecret = forceMaybeMsg clientSecretErrorMsg $ config ^. P.clientSecret
  rt <- loadRefreshToken
  A.getAccessToken clientId clientSecret rt
  where

-- | Load refresh token from file or request a new one from the Spotify API
loadRefreshToken :: IO RefreshToken
loadRefreshToken = do
  rt <- P.loadRefreshToken
  case rt of
    Just rt' -> return rt'
    Nothing -> do
      config <- P.loadConfig
      let clientId = forceMaybeMsg clientIdErrorMsg $ config ^. P.clientId
      let clientSecret = forceMaybeMsg clientSecretErrorMsg $ config ^. P.clientSecret
      ac <- A.getAuthorizationCode clientId
      rt' <- A.getRefreshToken clientId clientSecret ac
      P.saveRefreshToken rt'
      return rt'

clientIdErrorMsg :: String
clientIdErrorMsg = "failed to load clientId. ClientId has to be manually set in config.json. Please read README.md for further instructions"

clientSecretErrorMsg :: String
clientSecretErrorMsg = "failed to load clientSecret. ClientSecret has to be manually set in config.json. Please read README.md for further instructions"