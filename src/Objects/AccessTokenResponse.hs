{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Objects.AccessTokenResponse where

import           Control.Lens
import           Data.Aeson
import           Objects.AccessToken

data AccessTokenResponse = AccessTokenResponse
  { _accessToken :: AccessToken
  }
  deriving (Show)

instance FromJSON AccessTokenResponse where
  parseJSON (Object v) =
    AccessTokenResponse
      <$> (AccessToken <$> v .: "access_token")

$(makeLenses ''AccessTokenResponse)
