{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module AppState where

import ApiObjects.AccessToken (AccessToken)
import ApiObjects.Device (DeviceId)
import ApiObjects.Track (Track)
import Codec.Picture.Types (Image, PixelRGB8)
import Control.Lens
  ( assign,
    makeLenses,
    modifying,
    use,
    view,
    (%=),
    (.=),
  )
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State (StateT, evalStateT, execStateT)
import Data.Functor.Identity (runIdentity)

data AppState = AppState
  { _accessToken :: AccessToken,
    _isPlaying :: Bool,
    _deviceId :: Maybe DeviceId,
    _trackName :: Maybe String,
    _albumName :: Maybe String,
    _artistNames :: [String],
    _albumCover :: Image PixelRGB8,
    _showSearch :: Bool,
    _searchInput :: String,
    _searchResults :: [Track],
    _selectedSearchResultIndex :: Int
  }
  deriving (Show, Eq)

instance Show (Image a) where
  show _ = "<image>"

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
