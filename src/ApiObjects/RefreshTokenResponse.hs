{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ApiObjects.RefreshTokenResponse where

import ApiObjects.AccessToken (AccessToken (..))
import ApiObjects.RefreshToken (RefreshToken (..))
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))

data RefreshTokenResponse = RefreshTokenResponse
  { _accessToken :: AccessToken,
    _refreshToken :: RefreshToken
  }
  deriving (Show)

instance FromJSON RefreshTokenResponse where
  parseJSON = withObject "RefreshTokenResponse" $ \v ->
    RefreshTokenResponse
      <$> (AccessToken <$> v .: "access_token")
      <*> (RefreshToken <$> v .: "refresh_token")

$(makeLenses ''RefreshTokenResponse)
