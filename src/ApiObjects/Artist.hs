{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ApiObjects.Artist where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

-- | source https://developer.spotify.com/documentation/web-api/reference/object-model/#artist-object-simplified
data Artist = Artist
  { _artistId :: String,
    _artistName :: String
  }
  deriving (Show, Eq)

instance FromJSON Artist where
  parseJSON = withObject "Artist" $ \v ->
    Artist
      <$> (v .: "id")
      <*> (v .: "name")

$(makeLenses ''Artist)
