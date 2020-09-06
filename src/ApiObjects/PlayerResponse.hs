{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ApiObjects.PlayerResponse where

import ApiObjects.Device (Device)
import ApiObjects.Track (Track)
import Control.Applicative (optional)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

data PlayerResponse = PlayerResponse
  { _timestamp :: Int,
    _device :: Device,
    _progressMs :: Maybe String,
    _isPlaying :: Bool,
    _currentlyPlayingType :: String,
    _item :: Maybe Track,
    _shuffleState :: Bool,
    _repeatState :: String
  }
  deriving (Show)

instance FromJSON PlayerResponse where
  parseJSON = withObject "PlayerResponse" $ \v ->
    PlayerResponse
      <$> (v .: "timestamp")
      <*> (v .: "device")
      <*> optional (v .: "progress_ms")
      <*> (v .: "is_playing")
      <*> (v .: "currently_playing_type")
      <*> optional (v .: "item")
      <*> (v .: "shuffle_state")
      <*> (v .: "repeat_state")

$(makeLenses ''PlayerResponse)
