module ApiClientTest where

import           ApiClient                        (getCurrentAlbumCover,
                                                   getCurrentlyPlaying, pause,
                                                   play)
import           Authenticator                    (getAccessToken,
                                                   getAuthorizationCode,
                                                   getRefreshToken)
import           Codec.Picture.Extra              (scaleBilinear)
import           Data.Maybe                       (fromJust)
import           Network.HTTP.Types               (Status)
import           Objects.CurrentlyPlayingResponse (CurrentlyPlayingResponse)
import           Utils.ImageAnsiTerminalUtils     (imageToAnsi)

testPlay :: IO Status
testPlay = do
  at <- getAuthorizationCode >>= getRefreshToken >>= getAccessToken
  play at

testPause :: IO Status
testPause = do
  at <- getAuthorizationCode >>= getRefreshToken >>= getAccessToken
  pause at

testGetCurrentlyPlaying :: IO (Status, Maybe CurrentlyPlayingResponse)
testGetCurrentlyPlaying = do
  at <- getAuthorizationCode >>= getRefreshToken >>= getAccessToken
  getCurrentlyPlaying at

testGetCurrentAlbumCover :: IO ()
testGetCurrentAlbumCover = do
  at <- getAuthorizationCode >>= getRefreshToken >>= getAccessToken
  (status, image) <- getCurrentAlbumCover at
  let scaled = scaleBilinear 30 15 $ fromJust image
  putStrLn $ imageToAnsi scaled
