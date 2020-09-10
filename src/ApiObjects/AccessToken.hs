{-# LANGUAGE OverloadedStrings #-}

-- |
--  Author: Benedikt Spies
--
--  AccessToken representation of the Spotify Account Service.
module ApiObjects.AccessToken where

import GHC.Exts (IsString (..))
import Network.HTTP.Types.Header (Header)
import Utils.StringPack (Packable (..), Unpackable (..))

newtype AccessToken = AccessToken String
  deriving (Show, Eq)

instance IsString AccessToken where
  fromString s = pack s

instance Packable AccessToken where
  pack s = AccessToken s

instance Unpackable AccessToken where
  unpack (AccessToken s) = s

toAuthorizationHeader :: AccessToken -> Header
toAuthorizationHeader at = ("Authorization", pack $ "Bearer " ++ (unpack at))
