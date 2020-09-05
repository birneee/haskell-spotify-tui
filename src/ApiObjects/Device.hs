{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ApiObjects.Device where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

type DeviceId = String

type DeviceType = String

-- | source https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/#device-object
data Device = Device
  { _deviceId :: DeviceId,
    _isActive :: Bool,
    _deviceName :: String,
    _deviceType :: DeviceType,
    _volumePercent :: Int
  }
  deriving (Show)

instance FromJSON Device where
  parseJSON = withObject "Device" $ \v ->
    Device
      <$> (v .: "id")
      <*> (v .: "is_active")
      <*> (v .: "name")
      <*> (v .: "type")
      <*> (v .: "volume_percent")

$(makeLenses ''Device)
