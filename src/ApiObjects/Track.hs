{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ApiObjects.Track where

import           ApiObjects.Album  (Album)
import           ApiObjects.Artist (Artist)
import           Control.Lens      (makeLenses)
import           Data.Aeson        (FromJSON, Value, parseJSON, (.:))
import           Data.Aeson.Types  (Value (Object))

type Uri = String

-- |source https://developer.spotify.com/documentation/web-api/reference/object-model/#track-object-full
data Track = Track
  {
    _trackId     :: String,
    _diskNumber  :: Int,
    _durationMs  :: Int,
    _trackName   :: String,
    _popularity  :: Int,
    _trackNumber :: Int,
    _album       :: Album,
    _artists     :: [Artist],
    _uri         :: Uri
  }
  deriving (Show)

instance FromJSON Track where
  parseJSON (Object v) =
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
-- instance Eq Track where
--   -- (==) x y = x && y
--  Track {_trackId=a, _diskNumber=a1, _durationMs=a2, _trackName=a3,_popularity=a4, _trackNumber=a5,_album=a6,_artists=a7,_uri=a8} == Track {_trackId=b, _diskNumber=b1, _durationMs=b2, _trackName=b3,_popularity=b4, _trackNumber=b5,_album=b6,_artists=b7,_uri=b8} = compareString a b

-- compareString (x:xs) (y:ys) | x == y = True && compareString xs ys
--                             | not x == y = False 

$(makeLenses ''Track)

 