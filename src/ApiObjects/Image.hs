{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module ApiObjects.Image where

import           Control.Lens (makeLenses)
import           Data.Aeson   (FromJSON, Value (Object), parseJSON, (.:))

-- |source https://developer.spotify.com/documentation/web-api/reference/object-model/#image-object
data Image = Image
  {
    _url    :: String,
    _width  :: Int,
    _height :: Int
  }
  deriving(Show)

instance FromJSON Image where
  parseJSON (Object v) =
    Image
      <$> (v .: "url")
      <*> (v .: "width")
      <*> (v .: "height")

$(makeLenses ''Image)
