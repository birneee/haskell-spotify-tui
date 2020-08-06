{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Objects.RefreshTokenResponse where

import           Control.Lens
import           Data.Aeson
import           Objects.AccessToken
import           Objects.RefreshToken

data RefreshTokenResponse = RefreshTokenResponse
  { _accessToken  :: AccessToken,
    _refreshToken :: RefreshToken
  }
  deriving (Show)

instance FromJSON RefreshTokenResponse where
  parseJSON (Object v) =
    RefreshTokenResponse
      <$> (AccessToken <$> v .: "access_token")
      <*> (RefreshToken <$> v .: "refresh_token")

$(makeLenses ''RefreshTokenResponse)
