{-# LANGUAGE OverloadedStrings #-}

module Authenticator where

import           Control.Concurrent           (MVar, forkIO, killThread,
                                               newEmptyMVar, putMVar, takeMVar,
                                               threadDelay)
import           Control.Lens.Getter          ((^.))
import           Control.Lens.Operators       ((&))
import           Control.Lens.Setter          ((.~))
import           Data.Aeson                   (decode)
import           Data.Maybe                   (fromJust)
import           Network.HTTP.Client          (httpLbs, newManager,
                                               parseRequest_, responseBody)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Types           (status200, status400)
import           Network.Wai                  (Application, rawPathInfo,
                                               responseLBS)
import           Network.Wai.Handler.Warp     (defaultSettings, runSettings,
                                               setHost, setPort)
import           Objects.AccessToken          (AccessToken)
import           Objects.AccessTokenResponse  (accessToken)
import           Objects.AuthorizationCode    (AuthorizationCode)
import           Objects.RefreshToken         (RefreshToken)
import           Objects.RefreshTokenResponse (refreshToken)
import           Utils.HttpUtils              (getRequestQueryParam, toUrl)
import           Utils.RequestLenses          (method, queryString,
                                               urlEncodedBody)
import           Utils.ResponseLenses         (body)
import           Utils.StringUtils
import           Web.Browser                  (openBrowser)

-- TODO load from config file
clientId = "f6ee1f37d5ab4dfba595af9e7885e08c"

-- TODO load from config file
clientSecret = "adf60940a07f46908c3c457a4d147713"

redirectUrl = "http://localhost:8888/callback"

scopes = "user-modify-playback-state user-read-currently-playing" -- see all scopes at https://developer.spotify.com/documentation/general/guides/scopes/

getAuthorizationCode :: IO AuthorizationCode
getAuthorizationCode = do
  let request = parseRequest_ "https://accounts.spotify.com/authorize"
        & method .~ "GET"
        & queryString
        .~ [ ("client_id", Just clientId)
           , ("redirect_uri", Just "http://localhost:8888/callback")
           , ("response_type", Just "code")
           , ("scope", Just scopes)]
  --putStrLn $ "open this url in browser: " ++ toUrl request
  openBrowser $ toUrl request
  pack <$> awaitAuthorizationCallback

getRefreshToken :: AuthorizationCode -> IO RefreshToken
getRefreshToken ac = do
  let request = parseRequest_ "https://accounts.spotify.com/api/token"
        & method .~ "POST"
        & urlEncodedBody
        .~ [ ("grant_type", "authorization_code")
           , ("code", unpack ac)
           , ("redirect_uri", redirectUrl)
           , ("client_id", clientId)
           , ("client_secret", clientSecret)]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  let rawBody = response ^. body
  let tr = fromJust $ decode rawBody
  return $ tr ^. refreshToken

getAccessToken :: RefreshToken -> IO AccessToken
getAccessToken rt = do
  let request = parseRequest_ "https://accounts.spotify.com/api/token"
        & method .~ "POST"
        & urlEncodedBody
        .~ [ ("grant_type", "refresh_token")
           , ("refresh_token", unpack rt)
           , ("client_id", clientId)
           , ("client_secret", clientSecret)]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  let rawBody = response ^. body
  let tr = fromJust $ decode rawBody
  return $ tr ^. accessToken

awaitAuthorizationCallback :: IO String
awaitAuthorizationCallback = do
  codeMVar <- newEmptyMVar
  let settings = setPort 8888 $ setHost "127.0.0.1" $ defaultSettings -- allow only connections from localhost port 8888
  tid <- forkIO $ runSettings settings $ authorizationCallbackServer codeMVar
  code <- takeMVar codeMVar
  threadDelay 1000000 -- 1s
  killThread tid
  return code

authorizationCallbackServer :: MVar String -> Application
authorizationCallbackServer codeMVar request sendResponse = do
  let code = getRequestQueryParam "code" request
  let path = rawPathInfo request
  case (path, code) of
    ("/callback", Just code) -> do
      putMVar codeMVar code
      sendResponse
        $ responseLBS
          status200
          [("Content-Type", "text/plain")]
          "Login successful, browser can be closed"
    _ -> sendResponse
      $ responseLBS status400 [("Content-Type", "text/plain")] "Bad Request"
