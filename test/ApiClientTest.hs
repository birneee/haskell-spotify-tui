module ApiClientTest where

import           ApiClient                           (getAvailableDevices,
                                                      getCurrentAlbumCover,
                                                      getCurrentlyPlaying,
                                                      getPlayer, next, pause,
                                                      play, playTrack,
                                                      playTrackOnDevice,
                                                      previous, searchTrack,
                                                      setPlayer, setVolume)
import           ApiObjects.AccessToken              (AccessToken)
import           ApiObjects.CurrentlyPlayingResponse (CurrentlyPlayingResponse)
import           ApiObjects.Device                   (DeviceId)
import           ApiObjects.DevicesResponse          (DevicesResponse)
import           ApiObjects.PlayerResponse           (PlayerResponse)
import           ApiObjects.SearchResponse           (SearchResponse)
import           ApiObjects.Track                    (Uri)
import           Authenticator                       (getAccessToken,
                                                      getAuthorizationCode,
                                                      getRefreshToken)
import           Codec.Picture.Extra                 (scaleBilinear)
import           Data.Maybe                          (fromJust)
import           Network.HTTP.Types                  (Status)
import           Utils.ImageAnsiTerminalUtils        (imageToAnsi)
import           Utils.StringUtils

testPlay :: IO Status
testPlay = do
  accessToken >>= play

testPause :: IO Status
testPause = do
  accessToken >>= pause

testNext :: IO Status
testNext = do
  accessToken >>= next

testPrevious :: IO Status
testPrevious = do
  accessToken >>= previous

testGetCurrentlyPlaying :: IO (Status, Maybe CurrentlyPlayingResponse)
testGetCurrentlyPlaying = do
  accessToken >>= getCurrentlyPlaying

testGetCurrentAlbumCover :: IO ()
testGetCurrentAlbumCover = do
  (status, image) <- accessToken >>= getCurrentAlbumCover
  let scaled = scaleBilinear 30 30 $ fromJust image
  putStrLn $ imageToAnsi scaled

testGetAvailableDevices :: IO (Status, Maybe DevicesResponse)
testGetAvailableDevices = do
  accessToken >>= getAvailableDevices

testGetPlayer :: IO (Status, Maybe PlayerResponse)
testGetPlayer = do
  accessToken >>= getPlayer

testSetPlayer :: DeviceId -> IO Status
testSetPlayer deviceId = do
  at <- accessToken
  setPlayer at deviceId

testSetVolume :: Int -> IO Status
testSetVolume volumePercent = do
  at <- accessToken
  setVolume at volumePercent

testSearchTrack :: Uri -> IO (Status, Maybe SearchResponse)
testSearchTrack query = do
  at <- accessToken
  searchTrack at query

testPlayTrack :: Uri -> IO Status
testPlayTrack uri = do
  at <- accessToken
  playTrack at uri

testPlayTrackOnDevice :: Uri -> DeviceId -> IO Status
testPlayTrackOnDevice uri deviceId = do
  at <- accessToken
  playTrackOnDevice at uri deviceId

-- helper


accessToken :: IO AccessToken
-- |Get AccessToken by cached RefreshToken in file "refresh_token.tmp"
-- |TODO load from persistent json/yaml file
accessToken = do
  let filePath = "refresh_token.tmp"
  appendFile filePath ""
  content <- readFile filePath
  refreshToken <- case (length content) of
      0 -> do
        rt <- getAuthorizationCode >>= getRefreshToken
        writeFile filePath $ unpack rt
        return rt
      _ -> return $ pack content
  getAccessToken refreshToken
