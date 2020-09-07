{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (tuiMain) where

import qualified ApiObjects.Album as ALBUM (albumName)
import ApiObjects.Artist as ARTIST (artistName)
import ApiObjects.Track (Track, Uri, album, artists)
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
import qualified AppState as APPSTATE (albumName, artistNames, trackName)
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
    customMain,
    getVtyHandle,
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
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( viewport,
    visible,
    (<=>),
  )
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.List (listSelectedL)
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P
import Control.Lens (makeLenses, over, (%~), (&), (.~), (^.))
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
    updateCurrentTrackInfo,
  )
import Data.List (intercalate)
import qualified Data.Vector as Vec
import Graphics.Vty (Vty (refresh))
import qualified Graphics.Vty as V
import Utils.MaybeUtils ((?:))
import Widgets.ImageWidget (greedyRectangularImageWidget)

data Event
  = -- | forces vty to redraw album cover
    MarkAlbumCoverDirty

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

$(makeLenses ''SearchResultListItem)

data UIState = UIState
  { _edit :: E.Editor String Name, -- Search input
    _appState :: AppState,
    _results :: L.List Name SearchResultListItem,
    _eventChannel :: BChan Event
  }

$(makeLenses ''UIState)

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
      (selectedAttr, V.white `on` V.magenta),
      (shortcutAttr, V.black `on` V.white)
    ]

playAttr, stopAttr, nextAttr, previousAttr, pAttr, selectedAttr, progressCompleteAttr, progressIncompleteAttr :: AttrName
playAttr = "playAttr"
stopAttr = "stopAttr"
nextAttr = "nextAttr"
previousAttr = "previousAttr"
pAttr = "pAttr"
selectedAttr = "selectedAttr"
progressCompleteAttr = "progressComplete"
progressIncompleteAttr = "progressIncomplete"

shortcutAttr = "shortcutAttr"

app :: App UIState Event Name
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
  state <- newUIState
  let chan = state ^. eventChannel
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  _ <- customMain initialVty builder (Just chan) app state
  return ()

newUIState :: IO UIState
newUIState = do
  appState <- CONTROLLER.initAppState
  eventChannel <- newBChan 10
  return $ UIState (E.editor SearchEdit Nothing "") appState (L.list ResultList (Vec.fromList []) 1) eventChannel

drawUI :: UIState -> [Widget Name]
drawUI ui = [C.center $ drawMain ui]

drawMain :: UIState -> Widget Name
drawMain ui = vLimit 100 $ vBox [drawMusic ui, drawFunction ui, drawHelp]

drawMusic :: UIState -> Widget Name
drawMusic ui = withBorderStyle BS.unicode $ B.borderWithLabel (str "Haskell Spotify TUI") $ (C.center (drawAlbumCover ui) <+> B.vBorder <+> C.center (drawRight ui))

drawRight :: UIState -> Widget Name
drawRight ui
  | ui ^. appState ^. showSearch = drawSearch ui
  | otherwise = drawInfo ui

drawInfo :: UIState -> Widget n
drawInfo ui =
  C.center $
    C.padLeft (Pad 1) $
      vBox
        [ str $ "Track: " ++ (ui ^. (appState . APPSTATE.trackName) ?: ""),
          str $ "Artists: " ++ intercalate ", " (ui ^. (appState . APPSTATE.artistNames)),
          str $ "Album: " ++ (ui ^. (appState . APPSTATE.albumName) ?: ""),
          str $ "Review: "
        ]

drawAlbumCover :: UIState -> Widget Name
drawAlbumCover ui = do
  let image = ui ^. appState ^. albumCover
  B.border $ greedyRectangularImageWidget image

drawFunction :: UIState -> Widget Name
drawFunction ui =
  C.vLimit 3 $
    C.center $
      padRight (Pad 2) drawPrevious <+> padRight (Pad 2) drawStop <+> padRight (Pad 2) (drawPlay ui) <+> padRight (Pad 2) drawNext

drawSearch :: UIState -> Widget Name
drawSearch ui =
  str "Search " <+> (vLimit 1 $ E.renderEditor (str . unlines) True (ui ^. edit))
    <=> (C.strWrap "Track" <+> C.strWrap "Artists" <+> C.strWrap "Album")
    <=> viewport VPResultList T.Vertical (visible $ vLimit 20 $ drawResult ui) --TODO: call drawResult <=>

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
          else it --TODO: Check logic
   in selStr $
        C.hBox
          [ C.strWrap (item ^. trackName),
            C.strWrap $ intercalate ", " (item ^. artistNames),
            C.strWrap (item ^. albumName)
          ]

drawPlay :: UIState -> Widget Name
drawPlay ui
  | ui ^. appState ^. isPlaying = withAttr playAttr $ str "Play"
  | otherwise = withAttr pAttr $ str "Play"

drawProgressBar :: UIState -> Widget Name
drawProgressBar ui = P.progressBar (Just "X") 0

drawStop :: Widget n
drawStop = withAttr stopAttr $ str "Stop"

drawNext :: Widget n
drawNext = withAttr nextAttr $ str "Next"

drawPrevious :: Widget n
drawPrevious = withAttr previousAttr $ str "Previous"

drawHelp :: Widget n
drawHelp = C.hBox $ draw <$> help
  where
    draw :: (String, String) -> Widget n
    draw (shortcut, description) =
      padRight (Pad 3) $
        (withAttr shortcutAttr $ str shortcut)
          <+> (C.padLeft (Pad 1) $ str description)
    help :: [(String, String)]
    help =
      [ ("SPACE", "Play/Pause"),
        ("B", "Previous Track"),
        ("N", "Next Track"),
        ("ALT+F", "Search Track"),
        ("ESC", "Quit")
      ]

handleEvent :: UIState -> BrickEvent Name Event -> EventM Name (Next UIState)
handleEvent ui (AppEvent MarkAlbumCoverDirty) = do
  vty <- getVtyHandle
  liftIO $ refresh vty
  continue ui
handleEvent ui (VtyEvent (V.EvKey V.KDown [])) = do
  sendEvent MarkAlbumCoverDirty ui
  M.continue (ui & results %~ (\l -> L.listMoveDown l))
handleEvent ui (VtyEvent (V.EvKey V.KUp [])) = do
  sendEvent MarkAlbumCoverDirty ui
  M.continue (ui & results %~ (\l -> L.listMoveUp l))
handleEvent ui (VtyEvent (V.EvKey (V.KChar ' ') []))
  | ui ^. appState ^. isPlaying = exec CONTROLLER.pause ui
  | otherwise = exec CONTROLLER.play ui
handleEvent ui (VtyEvent (V.EvKey (V.KEsc) []))
  | ui ^. (appState . showSearch) = do
    sendEvent MarkAlbumCoverDirty ui
    continue $ ui & (appState . showSearch) .~ False
  | otherwise = halt ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'f') [V.MMeta])) = do
  sendEvent MarkAlbumCoverDirty ui
  continue $ over (appState . showSearch) not ui
handleEvent ui (VtyEvent (V.EvKey V.KEnter []))
  | ui ^. (appState . searchInput) == (head $ E.getEditContents $ ui ^. edit) = do
    let index = ui ^. (results . listSelectedL)
    case index of
      Just x -> do
        let ui' = ui & (appState . selectedSearchResultIndex) .~ x
        exec CONTROLLER.playSelectedTrack ui'
      Nothing -> continue ui
  | ui ^. (appState . showSearch) = do
    sendEvent MarkAlbumCoverDirty ui
    search ui
handleEvent ui (VtyEvent ev) | ui ^. appState ^. showSearch = continue =<< T.handleEventLensed ui edit E.handleEditorEvent ev -- for typing input
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
  sendEvent MarkAlbumCoverDirty ui
  exec CONTROLLER.next ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'b') [])) = do
  sendEvent MarkAlbumCoverDirty ui
  exec CONTROLLER.previous ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'm') [V.MMeta])) = do
  -- easter egg, Key: Alt + M
  sendEvent MarkAlbumCoverDirty ui
  exec CONTROLLER.mandelbrot ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'u') [])) = do
  sendEvent MarkAlbumCoverDirty ui
  exec CONTROLLER.updateCurrentTrackInfo ui
handleEvent ui _ = continue ui

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
