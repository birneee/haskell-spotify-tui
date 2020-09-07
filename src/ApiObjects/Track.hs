{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ApiObjects.Track where

import ApiObjects.Album (Album)
import ApiObjects.Artist (Artist)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

type Uri = String

-- | source https://developer.spotify.com/documentation/web-api/reference/object-model/#track-object-full
data Track = Track
  { _trackId :: String,
    _diskNumber :: Int,
    _durationMs :: Int,
    _trackName :: String,
    _popularity :: Int,
    _trackNumber :: Int,
    _album :: Album,
    _artists :: [Artist],
    _uri :: Uri
  }
  deriving (Show)

instance FromJSON Track where
  parseJSON = withObject "Track" $ \v ->
    Track
      <$> (v .: "id")
      <*> (v .: "disc_number")
      <*> (v .: "duration_ms")
      <*> (v .: "name")
      <*> (v .: "popularity")
      <*> (v .: "track_number")
      <*> (v .: "album")
      <*> (v .: "artists")
      <*> (v .: "uri")

$(makeLenses ''Track)
