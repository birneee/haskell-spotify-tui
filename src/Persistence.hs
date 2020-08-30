module Persistence where

import           ApiObjects.AccessToken (AccessToken)
import           Authenticator          (getAccessToken, getAuthorizationCode,
                                         getRefreshToken)
import           Utils.StringUtils

loadAccessToken :: IO AccessToken
-- |Get AccessToken by cached RefreshToken in file "refresh_token.tmp"
-- |TODO load from persistent json/yaml file
loadAccessToken = do
  let filePath = "refresh_token.tmp"
  appendFile filePath ""
  content <- readFile filePath
  refreshToken <- case (length content) of
      0 -> do
        rt <- getAuthorizationCode >>= getRefreshToken
        writeFile filePath $ unpack rt
        return rt
      _ -> return $ pack content
  getAccessToken refreshToken
