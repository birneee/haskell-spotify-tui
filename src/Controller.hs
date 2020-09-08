-- | TODO refresh access token if expired
-- TODO request new refresh token if not valid
module Controller where

import qualified ApiClient as API (setVolume, getAvailableDevices, getCurrentAlbumCover, getPlayer, next, pause, play, playTrack, previous, searchTrack, setPlayer)
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
import qualified ApiObjects.Track as TRACK (album, artists, durationMs, popularity, trackName)
import AppState
  ( AppState (AppState),
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
    progressMs,
    searchInput,
    searchResults,
    selectedSearchResultIndex,
    showSearch,
    trackName,
    trackPopularity,
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
    _progressMs,
    _searchInput,
    _searchResults,
    _selectedSearchResultIndex,
    _showSearch,
    _trackName,
    _trackPopularity,
  )
import qualified Authenticator as A
  ( getAccessToken,
    getAuthorizationCode,
    getRefreshToken,
  )
import Codec.Picture (Image, PixelRGB8)
import Control.Lens (ix, preuse, use, view, (.=), (^.), (^?), _Just)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Persistence as P
  ( loadRefreshToken,
    saveRefreshToken,
  )
import Utils.ImageGenerator (generateMandelbrotImage, generateRainbowImage)
import Utils.MaybeUtils ((?:))
import Utils.StatusLenses (code)

-- | placeholder that is displayed when no album cover is available
defaultAlbumCover :: Image PixelRGB8
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
        _durationMs = Nothing
      }

play :: AppStateIO ()
play = do
  at <- use accessToken
  status <- liftIO $ API.play at
  -- liftIO $ putStrLn $ show status
  case (status ^. code) of
    c | c `elem` [200, 202, 204] -> do
      isPlaying .= True
      updateCurrentTrackInfo
    _ -> return () -- TODO handle error

playSelectedTrack :: AppStateIO ()
playSelectedTrack = do
  index <- use selectedSearchResultIndex
  uri' <- preuse (searchResults . ix index . uri)
  at <- use accessToken
  status <- liftIO $ API.playTrack at (fromJust uri')
  -- liftIO $ putStrLn $ show status
  case (status ^. code) of
    c | c `elem` [200, 202, 204] -> do
      isPlaying .= True
      showSearch .= False
      updateCurrentTrackInfo
    _ -> return () -- TODO handle error

pause :: AppStateIO ()
pause = do
  at <- use accessToken
  status <- liftIO $ API.pause at
  case (status ^. code) of
    c | c `elem` [200, 202, 204] -> isPlaying .= False
    _ -> return () -- TODO handle error

next :: AppStateIO ()
next = do
  at <- use accessToken
  status <- liftIO $ API.next at
  -- liftIO $ putStrLn $ show status
  case (status ^. code) of
    c | c `elem` [200, 202, 204] -> do
      isPlaying .= True
      updateCurrentTrackInfo
    _ -> return () -- TODO handle error

previous :: AppStateIO ()
previous = do
  at <- use accessToken
  status <- liftIO $ API.previous at
  case (status ^. code) of
    c | c `elem` [200, 202, 204] -> do
      isPlaying .= True
      updateCurrentTrackInfo
    _ -> return () -- TODO handle error

-- | download albumCoverUrl and set albumCover
updateAlbumCover :: AppStateIO ()
updateAlbumCover = do
  at <- use accessToken
  (status, response) <- liftIO $ API.getCurrentAlbumCover at
  case (status ^. code, response) of
    (200, Just ac) -> albumCover .= ac
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
-- TODO calculate progress locally
updateProgress :: AppStateIO ()
updateProgress = do
  at <- use accessToken
  (_, response) <- liftIO $ API.getPlayer at
  case response of
    Just pr -> do
      progressMs .= pr ^. (PR.progressMs)
    _ -> do
      progressMs .= Nothing
      return () -- TODO handle error

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
        nextDeviceId = do
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
  if ac == defaultAlbumCover
    then albumCover .= generateMandelbrotImage 256 256
    else albumCover .= defaultAlbumCover

-- | Request new access token on Spotify API
requestAccessToken :: IO AccessToken
requestAccessToken = loadRefreshToken >>= A.getAccessToken

-- | Load refresh token from file or request a new one from the Spotify API
loadRefreshToken :: IO RefreshToken
loadRefreshToken = do
  rt <- P.loadRefreshToken
  case rt of
    Just rt' -> return rt'
    Nothing -> do
      rt' <- A.getAuthorizationCode >>= A.getRefreshToken
      P.saveRefreshToken rt'
      return rt'
