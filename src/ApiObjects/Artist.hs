{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ApiObjects.Artist where

import           Control.Lens (makeLenses)
import           Data.Aeson   (FromJSON, Value (Object), parseJSON, (.:))

-- |source https://developer.spotify.com/documentation/web-api/reference/object-model/#artist-object-simplified
data Artist = Artist
  {
    _artistId   :: String,
    _artistName :: String
  }
  deriving (Show)

instance FromJSON Artist where
  parseJSON (Object v) =
    Artist
      <$> (v .: "id")
      <*> (v .: "name")

$(makeLenses ''Artist)
