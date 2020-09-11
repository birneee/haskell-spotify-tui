{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
--  Author: Benedikt Spies
module ApiObjects.Image where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

-- | source https://developer.spotify.com/documentation/web-api/reference/object-model/#image-object
data Image = Image
  { _url :: String,
    _width :: Int,
    _height :: Int
  }
  deriving (Show)

instance FromJSON Image where
  parseJSON = withObject "Image" $ \v ->
    Image
      <$> (v .: "url")
      <*> (v .: "width")
      <*> (v .: "height")

$(makeLenses ''Image)
