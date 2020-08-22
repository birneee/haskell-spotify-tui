{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ApiObjects.AccessTokenResponse where

import           Control.Lens        (makeLenses)
import           Data.Aeson          (FromJSON, parseJSON, (.:))
import           Data.Aeson.Types    (Value (Object))
import           ApiObjects.AccessToken

data AccessTokenResponse = AccessTokenResponse
  { _accessToken :: AccessToken
  }
  deriving (Show)

instance FromJSON AccessTokenResponse where
  parseJSON (Object v) =
    AccessTokenResponse
      <$> (AccessToken <$> v .: "access_token")

$(makeLenses ''AccessTokenResponse)
