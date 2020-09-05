{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ApiObjects.AccessTokenResponse where

import ApiObjects.AccessToken (AccessToken (..))
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

data AccessTokenResponse = AccessTokenResponse
  { _accessToken :: AccessToken
  }
  deriving (Show)

instance FromJSON AccessTokenResponse where
  parseJSON = withObject "AccessTokenResponse" $ \v ->
    AccessTokenResponse
      <$> (AccessToken <$> v .: "access_token")

$(makeLenses ''AccessTokenResponse)
