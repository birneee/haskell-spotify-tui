{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ApiObjects.DevicesResponse where

import           ApiObjects.Device (Device)
import           Control.Lens      (makeLenses)
import           Data.Aeson        (FromJSON, parseJSON, (.:))
import           Data.Aeson.Types  (Value (Object))

data DevicesResponse = DevicesResponse
  {   _devices :: [Device]
  } deriving (Show)

instance FromJSON DevicesResponse where
  parseJSON (Object v) =
    DevicesResponse
      <$> (v .: "devices")

$(makeLenses ''DevicesResponse)
