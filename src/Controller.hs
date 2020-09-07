-- | TODO refresh access token if expired
--  TODO request new refresh token if not valid
module Controller (initAppState, play, search, requestAccessToken, updateCurrentTrackInfo, mandelbrot) where

import qualified ApiClient as API (getCurrentAlbumCover, getPlayer, pause, play)
import ApiObjects.AccessToken (AccessToken)
import qualified ApiObjects.Album as ALBUM (albumName, images)
import qualified ApiObjects.Artist as ARTIST (artistName)
import qualified ApiObjects.Device as DEVICE (deviceId)
import qualified ApiObjects.Image as IMAGE (url)
import qualified ApiObjects.PlayerResponse as PR (device, isPlaying, item)
import ApiObjects.RefreshToken (RefreshToken)
import qualified ApiObjects.Track as TRACK (album, artists, trackName)
import AppState
  ( AppState (AppState),
    AppStateIO,
    accessToken,
    albumCover,
    albumCoverUrl,
    albumName,
    artistNames,
    deviceId,
    isPlaying,
    trackName,
    _accessToken,
    _albumCover,
    _albumCoverUrl,
    _albumName,
    _artistNames,
    _deviceId,
    _isPlaying,
    _searchInput,
    _showSearch,
    _trackName,
  )
import qualified Authenticator as A
  ( getAccessToken,
    getAuthorizationCode,
    getRefreshToken,
  )
import Codec.Picture (Image, PixelRGB8)
import Control.Lens (Ixed (ix), assign, use, view, (.=), (^.), (^?))
import Control.Lens.Combinators (_Just)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Persistence as P
  ( loadRefreshToken,
    saveRefreshToken,
  )
import Utils.ImageGenerator (generateMandelbrotImage, generateRainbowImage)
import Utils.StatusLenses (code)

-- | placeholder that is displayed when no album cover is available
defaultAlbumCover :: Image PixelRGB8
defaultAlbumCover = generateRainbowImage

-- | TODO implement
search :: String -> AppStateIO ()
search s = undefined

initAppState :: IO AppState

-- | create a fresh AppState instance
-- this is called on app startup
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
        _albumCoverUrl = Nothing,
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
    _ -> return () -- TODO handle error

pause :: AppStateIO ()
pause = do
  at <- use accessToken
  status <- liftIO $ API.pause at
  case (status ^. code) of
    202 -> assign isPlaying False
    204 -> assign isPlaying False
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
  accessToken <- use accessToken
  (status, response) <- liftIO $ API.getPlayer accessToken
  case (status ^. code, response) of
    (200, Just pr) -> do
      isPlaying .= pr ^. (PR.isPlaying)
      deviceId .= Just (pr ^. (PR.device . DEVICE.deviceId))
      trackName .= pr ^? (PR.item . _Just . TRACK.trackName)
      albumName .= pr ^? (PR.item . _Just . TRACK.album . ALBUM.albumName)
      artistNames .= ((view ARTIST.artistName) <$> concat (pr ^? (PR.item . _Just . TRACK.artists)))
      albumCoverUrl .= pr ^? (PR.item . _Just . TRACK.album . ALBUM.images . ix 0 . IMAGE.url)
      updateAlbumCover
    _ -> return () -- TODO handle error

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
    Just rt -> return rt
    Nothing -> do
      rt <- A.getAuthorizationCode >>= A.getRefreshToken
      P.saveRefreshToken rt
      return rt
