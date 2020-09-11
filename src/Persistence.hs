-- |
--  Author: Yang Mao
--
--  Module for the config file handling
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
    decodeFileStrict,
    encode,
    object,
    pairs,
    withObject,
    (.:),
  )
import qualified Data.ByteString.Lazy as B
-- (❓) = self defined conditional (ternary) operator, (📖) = unpack, (📦) = pack
import Utils.UnicodeUtils ((❓), (📖), (📦))

-- | class for config object
data ConfigItem = ConfigItem
  { _clientId :: Maybe String, -- um die Lens unterscheiden
    _clientSecret :: Maybe String,
    _refreshToken :: Maybe RefreshToken
  }
  deriving (Show, Eq)

$(makeLenses ''ConfigItem)

-- | parse from JSON to ConfigItem Object
instance FromJSON ConfigItem where
  parseJSON = withObject "ConfigItem" $ \v ->
    ConfigItem
      <$> optional (v .: "clientId")
      <*> optional (v .: "clientSecret")
      <*> optional ((📦) <$> v .: "refreshToken")

-- | parse from ConfigItem to JSON string
instance ToJSON ConfigItem where
  toJSON (ConfigItem clientId' clientSecret' refreshToken') =
    object
      [ "clientId" .= clientId',
        "clientSecret" .= clientSecret',
        "refreshToken" .= ((📖) <$> refreshToken')
      ]
  toEncoding (ConfigItem clientId' clientSecret' refreshToken') =
    pairs
      ( "clientId" .= clientId'
          <> "clientSecret" .= clientSecret'
          <> "refreshToken" .= ((📖) <$> refreshToken')
      )

-- | Path to the config file
configFile :: FilePath
configFile = "config.json"

-- | save ConfigItem into config file
saveConfig :: ConfigItem -> IO ()
saveConfig config = B.writeFile configFile (encode config)

-- | File is created if it does not exist
loadConfig :: IO ConfigItem
loadConfig = do
  input <- try $ decodeFileStrict configFile :: IO (Either SomeException (Maybe ConfigItem))
  case input of
    Left _ -> saveConfig emptyConfig >> return emptyConfig
    Right input' -> return $ input' ❓ emptyConfig

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
