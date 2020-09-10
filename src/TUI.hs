{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Author: Kai-Chun Lin
-- Terminal based user interface (TUI) for the whole musik player which contains album cover, search-input field, play button and use-instruction
-- This TUI consists on the following components:
--   app: contains data type App s e n where s: state, e: event, n: name type
--   handleEvent: take state as input and return event
--   theMap: specifies attributes of TUI-elements

module TUI (tuiMain, UIState) where

import qualified ApiObjects.Album as ALBUM (albumName)
import ApiObjects.Artist as ARTIST (artistName)
import ApiObjects.Track (Track, album, artists)
import qualified ApiObjects.Track as TRACK (trackName)
import AppState
  ( AppState,
    AppStateIO,
    albumCover,
    deviceName,
    deviceVolumePercent,
    durationMs,
    execAppStateIO,
    isPlaying,
    progressMs,
    searchInput,
    searchResults,
    selectedSearchResultIndex,
    showSearch,
    unpackAlbumCover,
  )
import qualified AppState as APPSTATE (albumName, artistNames, trackName, trackPopularity)
import Brick (App (..), AttrMap, AttrName, BrickEvent (..), EventM, Next, Padding (..), Widget, attrMap, continue, customMain, getVtyHandle, halt, on, padRight, str, vBox, vLimit, withAttr, withBorderStyle, (<+>))
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
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens (makeLenses, over, (%~), (&), (.~), (^.))
import Control.Monad (forever, void)
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
    toggleDevice,
    updateCurrentTrackInfo,
    updateProgress,
    volumeDown,
    volumeUp,
  )
import Data.List (intercalate)
import qualified Data.Vector as Vec
import GHC.Float (int2Float)
import Graphics.Vty (Vty (refresh))
import qualified Graphics.Vty as V
import Text.Printf (printf)
import Utils.MaybeUtils ((?:))
import Widgets.ImageWidget (greedyRectangularImageWidget, greedyRectangularImageWidget240)
import Widgets.Right (right)

data Event
  = -- | forces vty to redraw album cover
    MarkAlbumCoverDirty
  | UpdateProgress

data Name
  = SearchEdit
  | ResultList
  | VPResultList
  deriving (Ord, Show, Eq)

data SearchResultListItem = SearchResultListItem
  { _trackName :: String,
    _albumName :: String,
    _artistNames :: [String]
    -- _trackUri :: Uri
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
      (E.editAttr, V.white `on` V.black),
      (pAttr, V.black `on` V.green),
      (selectedAttr, V.white `on` V.magenta),
      (shortcutAttr, V.black `on` V.white),
      (progressCompleteAttr, V.black `on` V.brightBlue),
      (progressIncompleteAttr, V.black `on` V.white)
    ]

playAttr, stopAttr, pAttr, selectedAttr, progressCompleteAttr, progressIncompleteAttr, shortcutAttr :: AttrName
playAttr = "playAttr"
stopAttr = "stopAttr"
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
  void . forkIO $
    forever $ do
      writeBChan chan UpdateProgress
      threadDelay 4000000 -- 2 seconds
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  _ <- customMain initialVty builder (Just chan) app state
  return ()

newUIState :: IO UIState
newUIState = do
  appState' <- CONTROLLER.initAppState
  eventChannel' <- newBChan 10
  return $ UIState (E.editor SearchEdit Nothing "") appState' (L.list ResultList (Vec.fromList []) 1) eventChannel'

drawUI :: UIState -> [Widget Name]
drawUI ui = [C.center $ drawMain ui]

drawMain :: UIState -> Widget Name
drawMain ui = vLimit 100 $ vBox [drawDevice ui, drawMusic ui, drawFunction ui, drawProgressBar ui, drawHelp]

drawDevice :: UIState -> Widget Name
drawDevice ui =
  right $
    str (ui ^. (appState . deviceName) ?: "No Device")
      <+> C.padLeft (Pad 2) (str ("Volume: " ++ show (ui ^. (appState . deviceVolumePercent) ?: 0) ++ "%"))

drawMusic :: UIState -> Widget Name
drawMusic ui = withBorderStyle BS.unicode $ B.borderWithLabel (str "Haskell Spotify TUI") $ (C.center (drawAlbumCover ui) <+> B.vBorder <+> C.center (drawRight ui))

drawRight :: UIState -> Widget Name
drawRight ui
  | ui ^. appState ^. showSearch = drawSearch ui
  | otherwise = drawInfo ui

drawInfo :: UIState -> Widget Name
drawInfo ui =
  C.center $
    C.padLeft (Pad 1) $
      vBox
        [ str $ "Track: " ++ (ui ^. (appState . APPSTATE.trackName) ?: ""),
          str $ "Artists: " ++ intercalate ", " (ui ^. (appState . APPSTATE.artistNames)),
          str $ "Album: " ++ (ui ^. (appState . APPSTATE.albumName) ?: ""),
          str "Popularity: " <+> drawPopularity ui
        ]

drawPopularity :: UIState -> Widget Name
drawPopularity ui =
  str $
    (take starNum $ repeat star)
      ++ (take (5 - starNum) $ repeat emptyStar)
  where
    starNum = round $ (fromIntegral popularity) / (20.0 :: Float)
    popularity = ui ^. (appState . APPSTATE.trackPopularity) ?: 0
    star = '★'
    emptyStar = '☆'

drawAlbumCover :: UIState -> Widget Name
drawAlbumCover ui = do
  let image = unpackAlbumCover $ ui ^. (appState . albumCover)
  B.border $
    if ui ^. (appState . showSearch)
      then greedyRectangularImageWidget240 image
      else greedyRectangularImageWidget image

drawFunction :: UIState -> Widget Name
drawFunction ui =
  C.vLimit 3 $
    C.center $
      padRight (Pad 2) (drawPlay ui)

drawSearch :: UIState -> Widget Name
drawSearch ui =
  C.padLeftRight 1 (str "Search " <+> (vLimit 1 $ E.renderEditor (str . unlines) True (ui ^. edit)))
    <=> (C.hCenter (str "Track") <+> C.hCenter (str "Artists") <+> C.hCenter (str "Album"))
    <=> viewport VPResultList T.Vertical (visible $ vLimit 20 $ drawResult ui)

trackToSearchResultListItem :: Track -> SearchResultListItem
trackToSearchResultListItem t =
  SearchResultListItem
    { _trackName = t ^. TRACK.trackName,
      _albumName = t ^. album ^. ALBUM.albumName,
      _artistNames = map (\a -> a ^. ARTIST.artistName) (t ^. artists)
      -- _trackUri = t ^. TRACK.uri
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
  | ui ^. appState ^. isPlaying = withAttr stopAttr $ str " ⏸ Stop "
  | otherwise = withAttr pAttr $ str " ▶ Play "

drawProgressBar :: UIState -> Widget Name
drawProgressBar ui = do
  let totalDuration = ui ^. (appState . durationMs) ?: 0
  let progressDuration = ui ^. (appState . progressMs) ?: 0
  let progress = int2Float progressDuration / int2Float totalDuration
  let text = millisecondsToText progressDuration ++ " / " ++ millisecondsToText totalDuration
  P.progressBar (Just text) progress
  where
    millisecondsToText :: Int -> String
    millisecondsToText t = do
      let totalSec = t `quot` 1000
      let (minutes, seconds) = totalSec `quotRem` 60
      printf "%d:%02d" minutes seconds

drawHelp :: Widget n
drawHelp = C.padTop (Pad 1) $ C.hBox $ draw <$> help
  where
    draw :: (String, String) -> Widget n
    draw (shortcut, description) =
      C.padRight (Pad 2) $
        (withAttr shortcutAttr $ str shortcut)
          <+> (C.padLeft (Pad 1) $ str description)
    help :: [(String, String)]
    help =
      [ ("SPACE", "Play/Pause"),
        ("P", "Previous Track"),
        ("N", "Next Track"),
        ("+", "Volume Up"),
        ("-", "Volume Down"),
        ("ALT+F", "Search Track"),
        ("ALT+D", "Change Device"),
        ("ESC", "Quit")
      ]

handleEvent :: UIState -> BrickEvent Name Event -> EventM Name (Next UIState)
handleEvent ui (AppEvent MarkAlbumCoverDirty) = do
  vty <- getVtyHandle
  liftIO $ refresh vty
  continue ui
handleEvent ui (VtyEvent (V.EvKey V.KDown [])) = continue (ui & results %~ (\l -> L.listMoveDown l))
handleEvent ui (VtyEvent (V.EvKey V.KUp [])) = continue (ui & results %~ (\l -> L.listMoveUp l))
handleEvent ui (VtyEvent (V.EvKey (V.KEsc) []))
  | ui ^. (appState . showSearch) = do
    sendEvent MarkAlbumCoverDirty ui
    continue $ ui & (appState . showSearch) .~ False
  | otherwise = halt ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'f') [V.MMeta])) = do
  sendEvent MarkAlbumCoverDirty ui
  continue $ over (appState . showSearch) not ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'd') [V.MMeta])) = do
  sendEvent MarkAlbumCoverDirty ui
  exec CONTROLLER.toggleDevice ui
handleEvent ui (VtyEvent (V.EvKey V.KEnter []))
  | ui ^. (appState . searchInput) == (head $ E.getEditContents $ ui ^. edit) = do
    let index = ui ^. (results . listSelectedL)
    case index of
      Just x -> do
        let ui' = ui & (appState . selectedSearchResultIndex) .~ x
        sendEvent MarkAlbumCoverDirty ui
        exec CONTROLLER.playSelectedTrack ui'
      Nothing -> continue ui
  | ui ^. (appState . showSearch) = do
    sendEvent MarkAlbumCoverDirty ui
    search ui
handleEvent ui (VtyEvent ev) | ui ^. appState ^. showSearch = continue =<< T.handleEventLensed ui edit E.handleEditorEvent ev -- for typing input
handleEvent ui (VtyEvent (V.EvKey (V.KChar ' ') []))
  | ui ^. appState ^. isPlaying = exec CONTROLLER.pause ui
  | otherwise = do
    sendEvent MarkAlbumCoverDirty ui
    exec CONTROLLER.play ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar '+') [])) = exec CONTROLLER.volumeUp ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar '-') [])) = exec CONTROLLER.volumeDown ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
  sendEvent MarkAlbumCoverDirty ui
  exec CONTROLLER.next ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'p') [])) = do
  sendEvent MarkAlbumCoverDirty ui
  exec CONTROLLER.previous ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'm') [V.MMeta])) = do
  -- easter egg, Key: Alt + M
  sendEvent MarkAlbumCoverDirty ui
  exec CONTROLLER.mandelbrot ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'u') [])) = do
  sendEvent MarkAlbumCoverDirty ui
  exec CONTROLLER.updateCurrentTrackInfo ui
handleEvent ui (AppEvent UpdateProgress) = exec CONTROLLER.updateProgress ui
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
