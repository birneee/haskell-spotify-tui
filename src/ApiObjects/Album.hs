{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ApiObjects.Album where

import           ApiObjects.Image (Image)
import           Control.Lens     (makeLenses)
import           Data.Aeson       (FromJSON, Value (Object), parseJSON, (.:))

-- |source https://developer.spotify.com/documentation/web-api/reference/object-model/#album-object-simplified
data Album = Album
  {
    _albumId   :: String,
    _albumName :: String,
    _images    :: [Image]
  }
  deriving (Show)

instance FromJSON Album where
  parseJSON (Object v) =
    Album
      <$>  (v .: "id")
      <*>  (v .: "name")
      <*>  (v .: "images")

$(makeLenses ''Album)
