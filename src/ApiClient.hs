{-# LANGUAGE OverloadedStrings #-}

-- | Spotify API https://developer.spotify.com/documentation/web-api/reference-beta
module ApiClient where

import           ApiObjects.AccessToken              (AccessToken,
                                                      toAuthorizationHeader)
import           ApiObjects.CurrentlyPlayingResponse (CurrentlyPlayingResponse,
                                                      item)
import           ApiObjects.DevicesResponse          (DevicesResponse)
import           ApiObjects.Item                     (album, images, url)
import           ApiObjects.PlayerResponse           (PlayerResponse)
import           Codec.Picture.Types                 (Image)
import           Codec.Picture.Types                 (PixelRGB8)
import           Control.Lens.Getter                 (view, (^.))
import           Control.Lens.Operators              ((&), (<&>))
import           Control.Lens.Setter                 ((.~))
import           Data.Aeson                          (decode, object, (.=))
import           Network.HTTP.Client                 (httpLbs, newManager,
                                                      parseRequest)
import           Network.HTTP.Client.TLS             (tlsManagerSettings)
import           Network.HTTP.Types                  (Status)
import           Utils.HttpUtils                     (getImage)
import           Utils.LensUtils                     ((?^.), (?^.?))
import           Utils.ListLenses                    (index)
import           Utils.RequestLenses                 (jsonBody, method,
                                                      queryString,
                                                      requestHeaders)
import           Utils.ResponseLenses                (body, status)

play :: AccessToken -> IO Status
play at = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/play"
        <&> method .~ "PUT"
        <&> requestHeaders .~ [toAuthorizationHeader at]
  -- print $ request
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  -- print $ response
  return $ response ^. status

pause :: AccessToken -> IO Status
pause at = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/pause"
        <&> method .~ "PUT"
        <&> requestHeaders .~ [toAuthorizationHeader at]
  -- print $ request
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  -- print $ response
  return $ response ^. status

next :: AccessToken -> IO Status
next at = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/next"
        <&> method .~ "POST"
        <&> requestHeaders .~ [toAuthorizationHeader at]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ response ^. status

previous :: AccessToken -> IO Status
previous at = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/previous"
        <&> method .~ "POST"
        <&> requestHeaders .~ [toAuthorizationHeader at]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ response ^. status

setVolume :: AccessToken -> Int -> IO Status
setVolume accessToken volumePercent = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/volume"
        <&> method .~ "PUT"
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
        <&> queryString .~ [("volume_percent", Just $ show volumePercent)]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ response ^. status

setPlayer :: AccessToken -> String -> IO Status
setPlayer accessToken deviceId = do
  request <- parseRequest "https://api.spotify.com/v1/me/player"
        <&> method .~ "PUT"
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
        <&> jsonBody .~ object ["device_ids" .= [deviceId]]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ response ^. status

getCurrentlyPlaying :: AccessToken -> IO (Status, Maybe CurrentlyPlayingResponse)
getCurrentlyPlaying at = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/currently-playing"
        <&> method .~ "GET"
        <&> requestHeaders .~ [toAuthorizationHeader at]
  -- print $ request
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  -- print $ response
  return (
      response ^. status,
      decode $ response ^. body)

getAvailableDevices :: AccessToken -> IO (Status, Maybe DevicesResponse)
getAvailableDevices accessToken = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/devices"
        <&> method .~ "GET"
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return (
    response ^. status,
    decode $ response ^. body)

getPlayer :: AccessToken -> IO (Status, Maybe PlayerResponse)
getPlayer accessToken = do
  request <- parseRequest "https://api.spotify.com/v1/me/player"
        <&> method .~ "GET"
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return (
    response ^. status,
    decode $ response ^. body)

getCurrentAlbumCover :: AccessToken -> IO (Status, Maybe (Image PixelRGB8))
getCurrentAlbumCover at = do
  (status, currentPlaying) <- getCurrentlyPlaying at
  let url = currentPlaying >>= anyAlbumUrl
  case url of
    Nothing  -> return (status, Nothing)
    Just url -> getImage url
  where
    anyAlbumUrl :: CurrentlyPlayingResponse -> Maybe String
    anyAlbumUrl response = do
      let img = response ^. item ?^. album ?^. images ?^.? (index 0)
      view url <$> img
