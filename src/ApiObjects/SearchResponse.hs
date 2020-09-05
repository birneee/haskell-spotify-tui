{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ApiObjects.SearchResponse where

import ApiObjects.Track (Track)
import Control.Applicative (optional)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

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
  parseJSON = withObject "SearchResponse" $ \v ->
    SearchResponse
      <$> optional (v .: "tracks")

instance FromJSON Tracks where
  parseJSON = withObject "Tracks" $ \v ->
    Tracks
      <$> (v .: "items")

$(makeLenses ''SearchResponse)
