{-# LANGUAGE OverloadedStrings #-}

module Authenticator where

import           Control.Concurrent           (MVar, forkIO, killThread,
                                               newEmptyMVar, putMVar, takeMVar,
                                               threadDelay)
import           Control.Lens.Getter          ((^.))
import           Data.Aeson                   (decode)
import           Data.Maybe                   (fromJust)
import           Network.HTTP.Client          (Request, httpLbs, method,
                                               newManager, parseRequest_,
                                               responseBody, setQueryString,
                                               urlEncodedBody)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Types           (status200, status400)
import           Network.Wai                  (Application, queryString,
                                               rawPathInfo, rawQueryString,
                                               responseLBS)
import           Network.Wai.Handler.Warp     (defaultSettings, runSettings,
                                               setHost, setPort)
import           Objects.AccessToken          (AccessToken)
import           Objects.AccessTokenResponse  (accessToken)
import           Objects.AuthorizationCode    (AuthorizationCode)
import           Objects.RefreshToken         (RefreshToken)
import           Objects.RefreshTokenResponse (refreshToken)
import           Utils.HttpUtils              (getRequestQueryParam, toUrl)
import           Utils.StringUtils
import           Web.Browser                  (openBrowser)

clientId = "f6ee1f37d5ab4dfba595af9e7885e08c"

clientSecret = "adf60940a07f46908c3c457a4d147713"

redirectUrl = "http://localhost:8888/callback"

scopes = "user-read-private user-read-email"

authorizeUrl = "https://accounts.spotify.com/authorize"

tokenUrl = "https://accounts.spotify.com/api/token"

getAuthorizationCode :: IO AuthorizationCode
getAuthorizationCode = do
  let request = setQueryString
        [ ("client_id", Just $ pack clientId)
        , ("redirect_uri", Just "http://localhost:8888/callback")
        , ("response_type", Just "code")]
        (parseRequest_ authorizeUrl)
  --putStrLn $ "open this url in browser: " ++ toUrl request
  openBrowser $ toUrl request
  pack <$> awaitAuthorizationCallback

getRefreshToken :: AuthorizationCode -> IO RefreshToken
getRefreshToken ac = do
  let request = urlEncodedBody
        [ ("grant_type", "authorization_code")
        , ("code", pack $ unpack ac)
        , ("redirect_uri", pack redirectUrl)
        , ("client_id", pack clientId)
        , ("client_secret", pack clientSecret)]
        (parseRequest_ tokenUrl) { method = "POST" }
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  let body = responseBody response
  let tr = fromJust $ decode body
  return $ tr ^. refreshToken

getAccessToken :: RefreshToken -> IO AccessToken
getAccessToken rt = do
  let request = urlEncodedBody
        [ ("grant_type", "refresh_token")
        , ("refresh_token", pack $ unpack rt)
        , ("client_id", pack clientId)
        , ("client_secret", pack clientSecret)]
        (parseRequest_ tokenUrl) { method = "POST" }
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  let body = responseBody response
  let tr = fromJust $ decode body
  return $ tr ^. accessToken

testAuthorization :: IO ()
testAuthorization = do
  ac <- getAuthorizationCode
  print $ ac
  rt <- getRefreshToken ac
  print $ rt
  at <- getAccessToken rt
  print at

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
