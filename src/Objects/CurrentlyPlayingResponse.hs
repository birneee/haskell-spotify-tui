{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Objects.CurrentlyPlayingResponse where

import           Control.Applicative (optional)
import           Control.Lens        (makeLenses)
import           Data.Aeson          (FromJSON, Object, parseJSON, (.:), (.:?))
import           Data.Aeson.Types    (Parser, Value (Object))
import           Data.Text

data CurrentlyPlayingResponse = CurrentlyPlayingResponse
  { _timestamp  :: Int,
    _progressMs :: Int,
    _isPlaying  :: Bool,
    _item       :: Item
  }
  deriving (Show)

data Item = Item
  {
    _itemId      :: String,
    _diskNumber  :: Int,
    _durationMs  :: Int,
    _itemName    :: String,
    _popularity  :: Int,
    _trackNumber :: Int,
    _album       :: Album,
    _artists     :: [Artist]
  }
  deriving (Show)

data Album = Album
  {
    _albumId   :: String,
    _albumName :: String,
    _images    :: [Image]
  }
  deriving (Show)

data Artist = Artist
  {
    _artistId   :: String,
    _artistName :: String
  }
  deriving (Show)

data Image = Image
  {
    _url    :: String,
    _width  :: Int,
    _height :: Int
  }
  deriving(Show)

instance FromJSON CurrentlyPlayingResponse where
  parseJSON (Object v) =
    CurrentlyPlayingResponse
      <$> (v .: "timestamp")
      <*> (v .: "progress_ms")
      <*> (v .: "is_playing")
      <*> (v .: "item")

instance FromJSON Item where
  parseJSON (Object v) =
    Item
      <$> (v .: "id")
      <*> (v .: "disc_number")
      <*> (v .: "duration_ms")
      <*> (v .: "name")
      <*> (v .: "popularity")
      <*> (v .: "track_number")
      <*> (v .: "album")
      <*> (v .: "artists")

instance FromJSON Album where
  parseJSON (Object v) =
    Album
      <$>  (v .: "id")
      <*>  (v .: "name")
      <*>  (v .: "images")

instance FromJSON Artist where
  parseJSON (Object v) =
    Artist
      <$> (v .: "id")
      <*> (v .: "name")

instance FromJSON Image where
  parseJSON (Object v) =
    Image
      <$> (v .: "url")
      <*> (v .: "width")
      <*> (v .: "height")

$(makeLenses ''CurrentlyPlayingResponse)
$(makeLenses ''Item)
$(makeLenses ''Album)
$(makeLenses ''Artist)
$(makeLenses ''Image)
