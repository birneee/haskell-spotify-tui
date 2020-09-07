module Persistence where

import ApiObjects.AccessToken (AccessToken)
import ApiObjects.RefreshToken (RefreshToken)
import Authenticator
  ( getAccessToken,
    getAuthorizationCode,
    getRefreshToken,
  )
import Utils.StringUtils

saveRefreshToken :: RefreshToken -> IO ()

-- | Save refresh token in file
--  |TODO use config.json file
saveRefreshToken refreshToken = do
  let filePath = "refresh_token.tmp"
  writeFile filePath $ unpack refreshToken

loadRefreshToken :: IO (Maybe RefreshToken)

-- | Get refresh token from file "refresh_token.tmp"
--  |TODO load from persistent config.json file
loadRefreshToken = do
  let filePath = "refresh_token.tmp"
  appendFile filePath ""
  content <- readFile filePath
  case (length content) of
    0 -> return Nothing
    _ -> return $ Just $ pack content
