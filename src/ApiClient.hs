{-# LANGUAGE OverloadedStrings #-}

module ApiClient where

import           Authenticator           (getAccessToken, getAuthorizationCode,
                                          getRefreshToken)
import           Network.HTTP.Client     (Request, httpLbs, method, newManager,
                                          parseRequest_, requestHeaders,
                                          responseBody, setQueryString)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Objects.AccessToken     (AccessToken)
import           Utils.StringUtils

play :: AccessToken -> IO ()
play at = do
  let url = "https://api.spotify.com/v1/me/player/play"
  let request = (parseRequest_ url) {
    method = "PUT",
    requestHeaders = [
      ("Authorization", pack $ "Bearer " ++ (unpack at))
    ]
  }
  -- print $ request
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  -- print $ response
  return ()

pause :: AccessToken -> IO ()
pause at = do
  let url = "https://api.spotify.com/v1/me/player/pause"
  let request = (parseRequest_ url) {
    method = "PUT",
    requestHeaders = [
      ("Authorization", pack $ "Bearer " ++ (unpack at))
    ]
  }
  -- print $ request
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  -- print $ response
  return ()

testPlay :: IO ()
testPlay = do
  at <- getAuthorizationCode >>= getRefreshToken >>= getAccessToken
  play at
  return ()

testPause :: IO ()
testPause = do
  at <- getAuthorizationCode >>= getRefreshToken >>= getAccessToken
  pause at
  return ()
