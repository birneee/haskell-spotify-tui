module Persistence where

import ApiObjects.RefreshToken (RefreshToken)
import Utils.StringUtils (Packable (pack), Unpackable (unpack))

-- | Save refresh token in file
-- TODO use config.json file
saveRefreshToken :: RefreshToken -> IO ()
saveRefreshToken refreshToken = do
  let filePath = "refresh_token.tmp"
  writeFile filePath $ unpack refreshToken

-- | Get refresh token from file "refresh_token.tmp"
-- TODO load from persistent config.json file
loadRefreshToken :: IO (Maybe RefreshToken)
loadRefreshToken = do
  let filePath = "refresh_token.tmp"
  appendFile filePath ""
  content <- readFile filePath
  case (length content) of
    0 -> return Nothing
    _ -> return $ Just $ pack content
