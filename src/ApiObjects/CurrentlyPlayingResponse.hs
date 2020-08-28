{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module ApiObjects.CurrentlyPlayingResponse where

import           ApiObjects.Track    (Track)
import           Control.Applicative (optional)
import           Control.Lens        (makeLenses)
import           Data.Aeson          (FromJSON, parseJSON, (.:), (.:?))
import           Data.Aeson.Types    (Value (Object))

data CurrentlyPlayingResponse = CurrentlyPlayingResponse
  { _timestamp  :: Int,
    _progressMs :: Maybe Int,
    _isPlaying  :: Bool,
    _item       :: Maybe Track
  }
  deriving (Show)

instance FromJSON CurrentlyPlayingResponse where
  parseJSON (Object v) =
    CurrentlyPlayingResponse
      <$> (v .: "timestamp")
      <*> optional (v .: "progress_ms")
      <*> (v .: "is_playing")
      <*> optional (v .: "item")

$(makeLenses ''CurrentlyPlayingResponse)
