{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module AppState where

import ApiObjects.AccessToken (AccessToken)
import ApiObjects.Device (DeviceId)
import ApiObjects.Track (Track)
import Codec.Picture.Types (Image, PixelRGB8)
import Control.Lens (makeLenses)
import Control.Monad.Trans.State (StateT, evalStateT, execStateT)
import Data.Functor.Identity (runIdentity)

data AppState = AppState
  { _accessToken :: AccessToken,
    _isPlaying :: Bool,
    _deviceId :: Maybe DeviceId,
    _deviceName :: Maybe String,
    _deviceType :: Maybe String,
    _deviceVolumePercent :: Maybe Int,
    _trackName :: Maybe String,
    _trackPopularity :: Maybe Int,
    _albumName :: Maybe String,
    _artistNames :: [String],
    _albumCoverUrl :: Maybe String,
    _albumCover :: AlbumCover,
    _showSearch :: Bool,
    _searchInput :: String,
    _searchResults :: [Track],
    _selectedSearchResultIndex :: Int,
    _progressMs :: Maybe Int,
    _durationMs :: Maybe Int,
    _latestLocalProgressUpdateTimestamp :: Maybe Int
  }
  deriving (Show)

newtype AlbumCover = AlbumCover (Image PixelRGB8) deriving (Eq)

unpackAlbumCover :: AlbumCover -> Image PixelRGB8
unpackAlbumCover (AlbumCover image) = image

packAlbumCover :: Image PixelRGB8 -> AlbumCover
packAlbumCover image = AlbumCover image

instance Show AlbumCover where
  show _ = "<albumCover>"

$(makeLenses ''AppState)

type AppStateT = StateT AppState

type AppStateM a = forall m. (Monad m) => AppStateT m a

type AppStateIO a = AppStateT IO a

evalAppState :: AppStateM a -> AppState -> a
evalAppState m = runIdentity . evalStateT m

execAppState :: AppStateM a -> AppState -> AppState
execAppState m = runIdentity . execStateT m

evalAppStateIO :: AppStateIO a -> AppState -> IO a
evalAppStateIO m = evalStateT m

execAppStateIO :: AppStateIO a -> AppState -> IO AppState
execAppStateIO m = execStateT m
