{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
--  Author: Benedikt Spies
module ApiObjects.CurrentlyPlayingResponse where

import ApiObjects.Track (Track)
import Control.Applicative (optional)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

data CurrentlyPlayingResponse = CurrentlyPlayingResponse
  { _timestamp :: Int,
    _progressMs :: Maybe Int,
    _isPlaying :: Bool,
    _item :: Maybe Track
  }
  deriving (Show)

instance FromJSON CurrentlyPlayingResponse where
  parseJSON = withObject "CurrentlyPlayingResponse" $ \v ->
    CurrentlyPlayingResponse
      <$> (v .: "timestamp")
      <*> optional (v .: "progress_ms")
      <*> (v .: "is_playing")
      <*> optional (v .: "item")

$(makeLenses ''CurrentlyPlayingResponse)
