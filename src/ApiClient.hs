{-# LANGUAGE OverloadedStrings #-}

-- | Spotify API https://developer.spotify.com/documentation/web-api/reference-beta
module ApiClient where

import Codec.Picture.Types              (Image)
import Codec.Picture.Types              (PixelRGB8)
import Control.Lens.Getter              ((^.))
import Control.Lens.Getter              (view)
import Control.Lens.Operators           ((&))
import Control.Lens.Setter              ((.~))
import Data.Aeson                       (decode)
import Network.HTTP.Client              (httpLbs, newManager,
                                                   parseRequest_)
import Network.HTTP.Client.TLS          (tlsManagerSettings)
import Network.HTTP.Types               (Status)
import Objects.AccessToken              (AccessToken,
                                                   toAuthorizationHeader)
import Objects.CurrentlyPlayingResponse (CurrentlyPlayingResponse,
                                                   album, images, item, url)
import Utils.HttpUtils                  (getImage)
import Utils.ListLenses                 (index)
import Utils.RequestLenses              (method, requestHeaders)
import Utils.ResponseLenses             (body, status)

play :: AccessToken -> IO Status
play at = do
  let request = parseRequest_ "https://api.spotify.com/v1/me/player/play"
        & method .~ "PUT"
        & requestHeaders .~ [toAuthorizationHeader at]
  -- print $ request
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  -- print $ response
  return $ response ^. status

pause :: AccessToken -> IO Status
pause at = do
  let request = parseRequest_ "https://api.spotify.com/v1/me/player/pause"
        & method .~ "PUT"
        & requestHeaders .~ [toAuthorizationHeader at]
  -- print $ request
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  -- print $ response
  return $ response ^. status

getCurrentlyPlaying :: AccessToken -> IO (Status, Maybe CurrentlyPlayingResponse)
getCurrentlyPlaying at = do
  let request = parseRequest_ "https://api.spotify.com/v1/me/player/currently-playing"
        & method .~ "GET"
        & requestHeaders .~ [toAuthorizationHeader at]
  -- print $ request
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  -- print $ response
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
      let img = response ^. (item . album . images . (index 0))
      view url <$> img
