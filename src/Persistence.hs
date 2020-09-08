{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Persistence where

import Control.Lens (makeLenses, (^.), (.~), (&))
import Utils.MaybeUtils ((?:))
import           ApiObjects.RefreshToken  (RefreshToken) 

import           Utils.StringUtils

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM

data ConfigItem = 
    ConfigItem {
        _clientId :: Maybe String -- um die Lens unterscheiden
      , _clientSecret :: Maybe String
      , _refreshToken :: Maybe RefreshToken} deriving(Show, Eq)

$(makeLenses ''ConfigItem)

instance FromJSON ConfigItem where
    parseJSON (Object v) =  ConfigItem
        <$> v .: "clientId"
        <*> v .: "clientSecret" 
        <*> (pack  <$> v .: "refreshToken")

instance ToJSON ConfigItem where
    toJSON (ConfigItem clientId' clientSecret' refreshToken') =
        object ["clientId" .= clientId'
        , "clientSecret" .= clientSecret'
        , "refreshToken" .= unpack refreshToken']
    toEncoding (ConfigItem clientId' clientSecret' refreshToken') =
        pairs ("clientId" .= clientId'
        <> "clientSecret" .= clientSecret'
        <> "refreshToken" .= unpack refreshToken')

configFile :: FilePath
-- configFile = "config.json"
configFile = "empty"

saveConfig :: ConfigItem -> IO ()
saveConfig config = B.writeFile configFile (encode config)
    
loadConfig :: IO ConfigItem
loadConfig = do 
    input <- B.readFile configFile
    return (decode input) ?: emptyConfig

emptyConfig :: ConfigItem
emptyConfig = ConfigItem Nothing Nothing Nothing

-- update :: RefreshToken -> ConfigItem -> RefreshToken -> Value
-- update refreshToken content = 
--   Object $ HM.insert "refreshToken" refreshToken content

saveRefreshToken :: RefreshToken -> IO ()
-- |Save refresh token in file
-- |TODO use config.json file
saveRefreshToken refreshToken' = do
  content <- loadConfig
  undefined
  -- let config' = case content of
  --     Nothing -> content 
  --     Just config -> content & refreshToken .~ refreshToken'
  --     update refreshToken content

  --     saveConfig config


-- | Get refresh token from file "refresh_token.tmp"
-- TODO load from persistent config.json file
loadRefreshToken :: IO (Maybe RefreshToken)
loadRefreshToken = do
  content <- loadConfig
  case content of
    Nothing -> return Nothing
    Just config -> return $ Just $ config ^. refreshToken
  -- return $ Just $ _refreshToken content
  -- let r' =  $ Just content ^. refreshToken
  -- return r' 
