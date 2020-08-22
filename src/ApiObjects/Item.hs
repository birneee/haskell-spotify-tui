{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ApiObjects.Item where

import           Control.Lens      (makeLenses)
import           Data.Aeson        (FromJSON, parseJSON, (.:))
import           Data.Aeson.Types  (Value (Object))

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

$(makeLenses ''Item)
$(makeLenses ''Album)
$(makeLenses ''Artist)
$(makeLenses ''Image)