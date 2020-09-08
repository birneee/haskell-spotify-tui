{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Persistence where

import ApiObjects.RefreshToken (RefreshToken)
import Control.Applicative (optional)
import Control.Exception.Base (SomeException, try)
import Control.Lens (makeLenses, (&), (.~), (^.))
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toEncoding, toJSON),
    Value (Object),
    decodeFileStrict,
    encode,
    object,
    pairs,
    (.:),
  )
import qualified Data.ByteString.Lazy as B
import Utils.MaybeUtils ((?:))
import Utils.StringUtils (Packable (pack), Unpackable (unpack))

data ConfigItem = ConfigItem
  { _clientId :: Maybe String, -- um die Lens unterscheiden
    _clientSecret :: Maybe String,
    _refreshToken :: Maybe RefreshToken
  }
  deriving (Show, Eq)

$(makeLenses ''ConfigItem)

instance FromJSON ConfigItem where
  parseJSON (Object v) =
    ConfigItem
      <$> optional (v .: "clientId")
      <*> optional (v .: "clientSecret")
      <*> optional (pack <$> v .: "refreshToken")

instance ToJSON ConfigItem where
  toJSON (ConfigItem clientId' clientSecret' refreshToken') =
    object
      [ "clientId" .= clientId',
        "clientSecret" .= clientSecret',
        "refreshToken" .= (unpack <$> refreshToken')
      ]
  toEncoding (ConfigItem clientId' clientSecret' refreshToken') =
    pairs
      ( "clientId" .= clientId'
          <> "clientSecret" .= clientSecret'
          <> "refreshToken" .= (unpack <$> refreshToken')
      )

configFile :: FilePath
configFile = "config.json"

saveConfig :: ConfigItem -> IO ()
saveConfig config = B.writeFile configFile (encode config)

loadConfig :: IO ConfigItem
loadConfig = do
  input <- try $ decodeFileStrict configFile :: IO (Either SomeException (Maybe ConfigItem))
  case input of
    Left _ -> return emptyConfig
    Right input' -> return $ input' ?: emptyConfig

-- | create an empty config
emptyConfig :: ConfigItem
emptyConfig = ConfigItem Nothing Nothing Nothing

-- | Save refresh token in file "config.json"
saveRefreshToken :: RefreshToken -> IO ()
saveRefreshToken refreshToken' = do
  content <- loadConfig
  let content' = content & refreshToken .~ Just refreshToken'
  saveConfig content'

-- | Get refresh token from file "config.json"
loadRefreshToken :: IO (Maybe RefreshToken)
loadRefreshToken = do
  content <- loadConfig
  return $ content ^. refreshToken
