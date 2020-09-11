{-# LANGUAGE OverloadedStrings #-}

module Authenticator where

import ApiObjects.AccessToken (AccessToken)
import ApiObjects.AccessTokenResponse (accessToken)
import ApiObjects.AuthorizationCode (AuthorizationCode)
import ApiObjects.RefreshToken (RefreshToken)
import ApiObjects.RefreshTokenResponse (refreshToken)
import Control.Concurrent
  ( MVar,
    forkIO,
    killThread,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
  )
import Control.Lens.Getter ((^.))
import Control.Lens.Operators ((<&>))
import Control.Lens.Setter ((.~))
import Data.Aeson (decode, eitherDecode)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Network.HTTP.Client
  ( httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (status200, status400)
import Network.Wai
  ( Application,
    rawPathInfo,
    responseLBS,
  )
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setHost,
    setPort,
  )
import Utils.HttpUtils (getRequestQueryParam, toUrl)
import Utils.RequestLenses
  ( method,
    queryString,
    urlEncodedBody,
  )
import Utils.ResponseLenses (body)
import Utils.StringUtils (Packable (pack), Unpackable (unpack))
import Web.Browser (openBrowser)

type ClientId = String

type ClientSecret = String

-- | TODO make port configurable
redirectUrl :: String
redirectUrl = "http://localhost:8888/callback"

-- | see all scopes at https://developer.spotify.com/documentation/general/guides/scopes/
scopes :: String
scopes =
  intercalate
    " "
    [ "user-modify-playback-state",
      "user-read-currently-playing",
      "user-read-playback-state"
    ]

getAuthorizationCode :: ClientId -> IO AuthorizationCode
getAuthorizationCode clientId = do
  request <-
    parseRequest "https://accounts.spotify.com/authorize"
      <&> method
      .~ "GET"
      <&> queryString
      .~ [ ("client_id", Just clientId),
           ("redirect_uri", Just "http://localhost:8888/callback"),
           ("response_type", Just "code"),
           ("scope", Just scopes)
         ]
  putStrLn $ "open url in browser, if not opened automatically: " ++ toUrl request
  _ <- openBrowser $ toUrl request
  pack <$> awaitAuthorizationCallback

getRefreshToken :: ClientId -> ClientSecret -> AuthorizationCode -> IO RefreshToken
getRefreshToken clientId clientSecret ac = do
  request <-
    parseRequest "https://accounts.spotify.com/api/token"
      <&> method
      .~ "POST"
      <&> urlEncodedBody
      .~ [ ("grant_type", "authorization_code"),
           ("code", unpack ac),
           ("redirect_uri", redirectUrl),
           ("client_id", clientId),
           ("client_secret", clientSecret)
         ]
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  let rawBody = response ^. body
  case eitherDecode rawBody of
    Left err -> error err
    Right tr -> return $ tr ^. refreshToken

getAccessToken :: ClientId -> ClientSecret -> RefreshToken -> IO AccessToken
getAccessToken clientId clientSecret rt = do
  request <-
    parseRequest "https://accounts.spotify.com/api/token"
      <&> method
      .~ "POST"
      <&> urlEncodedBody
      .~ [ ("grant_type", "refresh_token"),
           ("refresh_token", unpack rt),
           ("client_id", clientId),
           ("client_secret", clientSecret)
         ]
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
    ("/callback", Just code') -> do
      putMVar codeMVar code'
      sendResponse $
        responseLBS
          status200
          [("Content-Type", "text/plain")]
          "Login successful, browser can be closed"
    _ ->
      sendResponse $
        responseLBS status400 [("Content-Type", "text/plain")] "Bad Request"
