{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ApiObjects.Device where

import           Control.Lens     (makeLenses)
import           Data.Aeson       (FromJSON, parseJSON, (.:))
import           Data.Aeson.Types (Value (Object))

data Device = Device
  {   _id            :: String,
      _isActive      :: Bool,
      _name          :: String,
      _deviceType    :: String,
      _volumePercent :: Int
  } deriving (Show)

instance FromJSON Device where
  parseJSON (Object v) =
    Device
      <$> (v .: "id")
      <*> (v .: "is_active")
      <*> (v .: "name")
      <*> (v .: "type")
      <*> (v .: "volume_percent")

$(makeLenses ''Device)
