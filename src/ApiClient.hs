{-# LANGUAGE OverloadedStrings #-}

-- | Spotify API https://developer.spotify.com/documentation/web-api/reference-beta
module ApiClient where

import           ApiObjects.AccessToken              (AccessToken,
                                                      toAuthorizationHeader)
import           ApiObjects.Album                    (images)
import           ApiObjects.CurrentlyPlayingResponse (CurrentlyPlayingResponse,
                                                      item)
import           ApiObjects.Device                   (DeviceId)
import           ApiObjects.DevicesResponse          (DevicesResponse)
import           ApiObjects.Image                    (url)
import           ApiObjects.PlayerResponse           (PlayerResponse)
import           ApiObjects.SearchResponse           (SearchResponse)
import           ApiObjects.Track                    (Uri, album)
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
import           Utils.StringUtils                   (pack)

play :: AccessToken -> IO Status
play accessToken = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/play"
        <&> method .~ "PUT"
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ response ^. status

playTrack :: AccessToken -> Uri -> IO Status
playTrack accessToken uri = playTracks accessToken [uri]

playTracks :: AccessToken -> [Uri] -> IO Status
playTracks accessToken uris = playTracksOnDevice accessToken uris Nothing

playTrackOnDevice :: AccessToken -> Uri -> DeviceId -> IO Status
playTrackOnDevice accessToken uri deviceId = playTracksOnDevice accessToken [uri] (Just deviceId)

playTracksOnDevice :: AccessToken -> [Uri] -> (Maybe DeviceId) -> IO Status
playTracksOnDevice accessToken uris deviceId = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/play"
        <&> method .~ "PUT"
        <&> queryString .~ query
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
        <&> jsonBody .~ object ["uris" .= uris]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ response ^. status
  where
    query :: [(String, Maybe String)]
    query = case deviceId of
      Nothing        -> []
      Just deviceId' -> [("device_id", Just deviceId')]

pause :: AccessToken -> IO Status
pause accessToken = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/pause"
        <&> method .~ "PUT"
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ response ^. status

next :: AccessToken -> IO Status
next accessToken = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/next"
        <&> method .~ "POST"
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ response ^. status

previous :: AccessToken -> IO Status
previous accessToken = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/previous"
        <&> method .~ "POST"
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
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

setPlayer :: AccessToken -> DeviceId -> IO Status
setPlayer accessToken deviceId = do
  request <- parseRequest "https://api.spotify.com/v1/me/player"
        <&> method .~ "PUT"
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
        <&> jsonBody .~ object ["device_ids" .= [deviceId]]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ response ^. status

getCurrentlyPlaying :: AccessToken -> IO (Status, Maybe CurrentlyPlayingResponse)
getCurrentlyPlaying accessToken = do
  request <- parseRequest "https://api.spotify.com/v1/me/player/currently-playing"
        <&> method .~ "GET"
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
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
getCurrentAlbumCover accessToken = do
  (status, currentPlaying) <- getCurrentlyPlaying accessToken
  let url = currentPlaying >>= anyAlbumUrl
  case url of
    Nothing  -> return (status, Nothing)
    Just url -> getImage url
  where
    anyAlbumUrl :: CurrentlyPlayingResponse -> Maybe String
    anyAlbumUrl response = do
      let img = response ^. item ?^. album ?^. images ?^.? (index 0)
      view url <$> img

searchTrack :: AccessToken -> String -> IO (Status, Maybe SearchResponse)
searchTrack accessToken query = do
  request <- parseRequest "https://api.spotify.com/v1/search"
        <&> method .~ "GET"
        <&> requestHeaders .~ [toAuthorizationHeader accessToken]
        <&> queryString .~ [
          ("q", Just query),
          ("type", Just "track")]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return (
    response ^. status,
    decode $ response ^. body)
