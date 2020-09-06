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
    albumCover,
    execAppStateIO,
    isPlaying,
    searchInput,
    searchResults,
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
    attrName,
    continue,
    customMain,
    defaultMain,
    emptyWidget,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padRight,
    padTop,
    resizeOrQuit,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import Brick.AttrMap (AttrMap, AttrName, attrMap)
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
import Brick.Widgets.List (GenericList)
import qualified Brick.Widgets.List as L
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Controller as CONTROLLER
  ( initAppState,
    next,
    pause,
    play,
    previous,
    search,
  )
import Data.Char
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Widgets.ImageWidget (greedyRectangularImageWidget)

data State = String

data Event = Event

data Tick = Tick

-- type Name = ()

data Name
  = SearchEdit
  | ResultList
  | VPResultList
  deriving (Ord, Show, Eq)

-- data Name1 = TextBox
--   deriving (Show, Ord, Eq)
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
    _results :: L.List Name SearchResultListItem
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
  return $ UIState (E.editor SearchEdit Nothing "") appState (L.list ResultList (Vec.fromList []) 1)

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
-- listDrawElement b item = C.hCenter $ str $ item ^. trackName
listDrawElement b item =
  let selStr it =
        if b
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

drawHelp = str $ "'p':PLAY, 's':STOP, 'p':BACK, 'n':NEXT, 'esc':QUIT"

handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)
handleEvent ui (VtyEvent (V.EvKey (V.KChar ' ') []))
  | ui ^. appState ^. isPlaying = pause ui
  | otherwise = play ui
handleEvent ui (VtyEvent (V.EvKey (V.KEsc) [])) = halt ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'f') [V.MMeta])) = continue $ over (appState . showSearch) not ui
handleEvent ui (VtyEvent (V.EvKey V.KEnter [])) | ui ^. appState ^. showSearch = do
  search ui
handleEvent ui (VtyEvent ev) | ui ^. appState ^. showSearch = continue =<< T.handleEventLensed ui edit E.handleEditorEvent ev -- for typing input
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'n') [])) = next ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'b') [])) = previous ui
handleEvent ui _ = continue ui

setPlayerModus :: String -> UIState -> EventM Name (Next UIState)
setPlayerModus "play" ui = do
  let a = ui ^. appState
  a' <- liftIO $ execAppStateIO CONTROLLER.play a
  liftIO $ putStrLn $ show a'
  continue $ ui & appState .~ a'
setPlayerModus "search" ui =
  let a = ui ^. appState
      u = execAppStateIO CONTROLLER.search a
      ui . appState = u
   in continue ui
setPlayerModus "pause" ui =
  let a = ui ^. appState
      u = execAppStateIO CONTROLLER.pause a
      ui . appState = u
   in continue ui
setPlayerModus "next" ui =
  let a = ui ^. appState
      u = execAppStateIO CONTROLLER.next a
      ui . appState = u
   in continue ui
setPlayerModus "previous" ui =
  let a = ui ^. appState
      u = execAppStateIO CONTROLLER.previous a
      ui . appState = u
   in continue ui

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
  liftIO $ putStrLn $ show a'
  continue $ ui & appState .~ a'

next :: UIState -> EventM Name (Next UIState)
next ui = do
  let a = ui ^. appState
  a' <- liftIO $ execAppStateIO CONTROLLER.next a
  liftIO $ putStrLn $ show a'
  continue $ ui & appState .~ a'

previous :: UIState -> EventM Name (Next UIState)
previous ui = do
  let a = ui ^. appState
  a' <- liftIO $ execAppStateIO CONTROLLER.previous a
  liftIO $ putStrLn $ show a'
  continue $ ui & appState .~ a'

-- (makeLenses ''SearchResultListItem)
