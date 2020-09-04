{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ApiObjects.SearchResponse where

import ApiObjects.Device (Device)
import ApiObjects.Track (Track)
import Control.Applicative (optional)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, parseJSON, (.:))
import Data.Aeson.Types (Value (Object))

data SearchResponse = SearchResponse
  { _tracks :: Maybe Tracks
  -- _artists :: Maybe Artists
  -- _albums :: Maybe Albums
  -- _playlists :: Maybe Playlists
  }
  deriving (Show)

data Tracks = Tracks
  { _items :: [Track]
  }
  deriving (Show)

instance FromJSON SearchResponse where
  parseJSON (Object v) =
    SearchResponse
      <$> optional (v .: "tracks")

instance FromJSON Tracks where
  parseJSON (Object v) =
    Tracks
      <$> (v .: "items")

$(makeLenses ''SearchResponse)
$(makeLenses ''Tracks)
