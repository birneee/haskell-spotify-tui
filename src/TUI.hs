{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (tuiMain) where

import qualified ApiObjects.Album as ALBUM (albumName)
import ApiObjects.Artist as ARTIST (artistName)
import ApiObjects.Track (Track, Uri, album, artists, trackId)
import qualified ApiObjects.Track as TRACK (trackName, uri)
import AppState
  ( AppState,
    AppStateIO,
    albumCover,
    execAppStateIO,
    isPlaying,
    searchInput,
    searchResults,
    selectedSearchResultIndex,
    showSearch,
  )
import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Next,
    Padding (..),
    Widget,
    attrMap,
    continue,
    defaultMain,
    halt,
    on,
    padRight,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Brick.Focus as F
import qualified Brick.Main as M
import Brick.Types
  ( Extent,
    Location,
    Padding (..),
    Widget,
  )
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hBox,
    hLimit,
    str,
    txt,
    updateAttrMap,
    vLimit,
    viewport,
    visible,
    withAttr,
    withBorderStyle,
    (<+>),
    (<=>),
  )
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.List (GenericList, listSelectedL)
import qualified Brick.Widgets.List as L
import Control.Lens (makeLenses, over, (%~), (&), (.~), (^.))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Controller as CONTROLLER
  ( initAppState,
    mandelbrot,
    next,
    pause,
    play,
    playSelectedTrack,
    previous,
    search,
  )
import Data.Char
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Graphics.Vty (refresh)
import qualified Graphics.Vty as V
import Widgets.ImageWidget (greedyRectangularImageWidget)

data State = String

data Event
  = -- | forces vty to redraw album cover
    MarkAlbumCoverDirty

data Tick = Tick

-- type Name = ()

data Name
  = SearchEdit
  | ResultList
  | VPResultList
  deriving (Ord, Show, Eq)

data SearchResultListItem = SearchResultListItem
  { _trackName :: String,
    _albumName :: String,
    _artistNames :: [String],
    _trackUri :: Uri
  }

(makeLenses ''SearchResultListItem)

data UIState = UIState
  { _edit :: E.Editor String Name, -- Search input
    _appState :: AppState,
    _results :: L.List Name SearchResultListItem,
    _eventChannel :: BChan Event
  }

(makeLenses ''UIState)

--TODO: Nebenlaeufigkeit
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (playAttr, V.white `on` V.green),
      (stopAttr, V.white `on` V.red),
      (nextAttr, V.white `on` V.blue),
      (previousAttr, V.white `on` V.cyan),
      (E.editAttr, V.white `on` V.black),
      (pAttr, V.black `on` V.green),
      (selectedAttr, V.white `on` V.magenta)
    ]

playAttr, stopAttr, nextAttr, previousAttr, pAttr, selectedAttr :: AttrName
playAttr = "playAttr"
stopAttr = "stopAttr"
nextAttr = "nextAttr"
previousAttr = "previousAttr"
pAttr = "pAttr"
selectedAttr = "selectedAttr"

app :: App UIState () Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = M.showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

tuiMain :: IO ()
tuiMain = do
  u <- newUIState
  defaultMain app u
  return ()

newUIState :: IO UIState
newUIState = do
  appState <- CONTROLLER.initAppState
  eventChannel <- newBChan 10
  return $ UIState (E.editor SearchEdit Nothing "") appState (L.list ResultList (Vec.fromList []) 1) eventChannel

drawUI :: UIState -> [Widget Name]
drawUI ui = [C.center $ drawMain ui]

drawMain ui = vLimit 100 $ vBox [drawMusic ui, drawFunction ui, drawHelp]

drawMusic :: UIState -> Widget Name
drawMusic ui = withBorderStyle BS.unicode $ B.borderWithLabel (str "FFP Music Player") $ (C.center (drawAlbumCover ui) <+> B.vBorder <+> C.center (drawRight ui))

drawRight :: UIState -> Widget Name
drawRight ui
  | ui ^. appState ^. showSearch = drawSearch ui
  | otherwise = drawInfo ui

drawInfo :: UIState -> Widget Name
drawInfo ui = vBox [C.center (str "Title"), C.center (str "Song"), C.center (str "Artist"), C.center (str "Review")]

drawAlbumCover :: UIState -> Widget Name
drawAlbumCover ui = do
  let image = ui ^. appState ^. albumCover
  B.border $ greedyRectangularImageWidget image

drawFunction :: UIState -> Widget Name
drawFunction ui = padRight (Pad 2) drawPrevious <+> padRight (Pad 2) drawStop <+> padRight (Pad 2) (drawPlay ui) <+> padRight (Pad 2) drawNext

drawSearch :: UIState -> Widget Name
drawSearch ui =
  str "Input " <+> (vLimit 1 $ E.renderEditor (str . unlines) True (ui ^. edit)) <=> viewport VPResultList T.Vertical (visible $ vLimit 20 $ drawResult ui) --TODO: call drawResult <=>

trackToSearchResultListItem :: Track -> SearchResultListItem
trackToSearchResultListItem t =
  SearchResultListItem
    { _trackName = t ^. TRACK.trackName,
      _albumName = t ^. album ^. ALBUM.albumName,
      _artistNames = map (\a -> a ^. ARTIST.artistName) (t ^. artists),
      _trackUri = t ^. TRACK.uri
    }

drawResult :: UIState -> Widget Name
drawResult ui = do
  let genericList = ui ^. results
  L.renderList listDrawElement True genericList

listDrawElement :: Bool -> SearchResultListItem -> Widget Name
listDrawElement sel item =
  let selStr it =
        if sel
          then withAttr selectedAttr (it)
          else it --TDO: Check logic
   in selStr $ str (item ^. trackName)

drawPlay :: UIState -> Widget Name
drawPlay ui
  | ui ^. appState ^. isPlaying = withAttr playAttr $ str "Play"
  | otherwise = withAttr pAttr $ str "Play"

drawStop = withAttr stopAttr $ str "Stop"

drawNext = withAttr nextAttr $ str "Next"

drawPrevious = withAttr previousAttr $ str "Previous"

drawHelp = str "Please log in Spotify at first" <=> str "'space':PLAY/ STOP, 'b':BACK, 'n':NEXT, 'esc':QUIT, 'alt-f': turn-off the input field"

handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)
handleEvent ui (VtyEvent (V.EvKey V.KDown [])) = M.continue (ui & results %~ (\l -> L.listMoveDown l))
handleEvent ui (VtyEvent (V.EvKey V.KUp [])) = M.continue (ui & results %~ (\l -> L.listMoveUp l))
handleEvent ui (VtyEvent (V.EvKey (V.KChar ' ') []))
  | ui ^. appState ^. isPlaying = pause ui
  | otherwise = play ui
handleEvent ui (VtyEvent (V.EvKey (V.KEsc) [])) = halt ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'f') [V.MMeta])) = continue $ over (appState . showSearch) not ui
handleEvent ui (VtyEvent (V.EvKey V.KEnter []))
  | ui ^. (appState . searchInput) == (head $ E.getEditContents $ ui ^. edit) = do
    let index = ui ^. (results . listSelectedL)
    case index of
      Just x -> do
        let ui' = ui & (appState . selectedSearchResultIndex) .~ x
        exec CONTROLLER.playSelectedTrack ui'
      Nothing -> continue ui
  | ui ^. (appState . showSearch) = do search ui
handleEvent ui (VtyEvent ev) | ui ^. appState ^. showSearch = continue =<< T.handleEventLensed ui edit E.handleEditorEvent ev -- for typing input
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'n') [])) = next ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'b') [])) = previous ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'm') [V.MMeta])) = do
  -- easter egg, Key: Alt + M
  sendEvent MarkAlbumCoverDirty ui
  exec CONTROLLER.mandelbrot ui
handleEvent ui _ = continue ui

play :: UIState -> EventM Name (Next UIState)
play ui = do
  let a = ui ^. appState
  a' <- liftIO $ execAppStateIO CONTROLLER.play a
  liftIO $ putStrLn $ show a'
  continue $ ui & appState .~ a'

search :: UIState -> EventM Name (Next UIState)
search ui = do
  let content = head $ E.getEditContents $ ui ^. edit
  -- liftIO $ putStrLn content
  let ui' = ui & (appState . searchInput) .~ content
  as <- liftIO $ execAppStateIO CONTROLLER.search (ui' ^. appState) -- newAppState
  let ui'' = ui' & appState .~ as
  let items = trackToSearchResultListItem <$> as ^. searchResults
  let ui''' = ui'' & results .~ L.list ResultList (Vec.fromList items) 1
  -- liftIO $ putStrLn $ show as
  continue ui'''

pause :: UIState -> EventM Name (Next UIState)
pause ui = do
  let a = ui ^. appState
  a' <- liftIO $ execAppStateIO CONTROLLER.pause a
  -- liftIO $ putStrLn $ show a'
  continue $ ui & appState .~ a'

next :: UIState -> EventM Name (Next UIState)
next ui = do
  let a = ui ^. appState
  a' <- liftIO $ execAppStateIO CONTROLLER.next a
  -- liftIO $ putStrLn $ show a'
  continue $ ui & appState .~ a'

previous :: UIState -> EventM Name (Next UIState)
previous ui = do
  let a = ui ^. appState
  a' <- liftIO $ execAppStateIO CONTROLLER.previous a
  -- liftIO $ putStrLn $ show a'
  continue $ ui & appState .~ a'

-- | send an event to the brick event queue
sendEvent :: Event -> UIState -> EventM Name ()
sendEvent event ui = do
  let chan = ui ^. eventChannel
  liftIO $ writeBChan chan event
  return ()

eval :: AppStateIO a -> UIState -> EventM Name UIState
eval f uiState = do
  let as = uiState ^. appState
  as' <- liftIO $ execAppStateIO f as
  return $ uiState & appState .~ as'

exec :: AppStateIO a -> UIState -> EventM Name (Next UIState)
exec f uiState = eval f uiState >>= continue
