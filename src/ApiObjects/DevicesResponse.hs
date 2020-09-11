{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
--  Author: Benedikt Spies
module ApiObjects.DevicesResponse where

import ApiObjects.Device (Device)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

data DevicesResponse = DevicesResponse
  { _devices :: [Device]
  }
  deriving (Show)

instance FromJSON DevicesResponse where
  parseJSON = withObject "DevicesResponse" $ \v ->
    DevicesResponse
      <$> (v .: "devices")

$(makeLenses ''DevicesResponse)
