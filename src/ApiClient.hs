{-# LANGUAGE OverloadedStrings #-}

module ApiClient where

import           Authenticator (getAccessToken, getAuthorizationCode
                              , getRefreshToken)
import           Network.HTTP.Client (Request, httpLbs, newManager
                                    , parseRequest_)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Objects.AccessToken (AccessToken, toAuthorizationHeader)
import           Utils.StringUtils
import           Control.Lens.Operators ((&))
import           Control.Lens.Setter ((.~))
import           Utils.RequestLenses (method, requestHeaders)

play :: AccessToken -> IO ()
play at = do
  let request = parseRequest_ "https://api.spotify.com/v1/me/player/play"
        & method .~ "PUT"
        & requestHeaders .~ [toAuthorizationHeader at]
  -- print $ request
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  -- print $ response
  return ()

pause :: AccessToken -> IO ()
pause at = do
  let request = parseRequest_ "https://api.spotify.com/v1/me/player/pause"
        & method .~ "PUT"
        & requestHeaders .~ [toAuthorizationHeader at]
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
