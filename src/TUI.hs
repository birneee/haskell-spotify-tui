{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (tuiMain) where

import AppState (AppState, AppStateIO, albumCover, execAppStateIO)
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
    hBox,
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
import Brick.AttrMap (AttrMap, AttrName, attrMap)
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Brick.Main as M
import Brick.Types
  ( Padding (..),
    Widget,
  )
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
    visible,
    withAttr,
    withBorderStyle,
    (<+>),
    (<=>),
  )
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.Edit as E
import Control.Lens (makeLenses, (&), (.~), (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Controller as CONTROLLER (initAppState, mandelbrot, play)
import Graphics.Vty (refresh)
import qualified Graphics.Vty as V
import Widgets.ImageWidget (greedyRectangularImageWidget)

data Event
  = -- | forces vty to redraw album cover
    MarkAlbumCoverDirty

type Name = ()

data UIState = UIState
  { -- | Search input
    _edit :: E.Editor String Name,
    _appState :: AppState,
    _eventChannel :: BChan Event
  }

makeLenses ''UIState

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (playAttr, V.white `on` V.green),
      (stopAttr, V.white `on` V.red),
      (nextAttr, V.white `on` V.blue),
      (previousAttr, V.white `on` V.cyan)
    ]

playAttr, stopAttr, nextAttr, previousAttr :: AttrName
playAttr = "playAttr"
stopAttr = "stopAttr"
nextAttr = "nextAttr"
previousAttr = "previousAttr"

app :: App UIState Event Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = M.neverShowCursor,
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
  customMain initialVty builder (Just chan) app state
  return ()

newUIState :: IO UIState
newUIState = do
  appState <- CONTROLLER.initAppState
  eventChannel <- newBChan 10
  return $
    UIState
      (E.editor () Nothing "")
      appState
      eventChannel

drawUI :: UIState -> [Widget Name]
drawUI ui = [C.center $ drawMain ui, drawSearch True]

drawMain :: UIState -> Widget n
drawMain ui = vLimit 100 $ vBox [drawMusic ui <=> drawFunction, str $ "'p':PLAY, 's':STOP, 'p':BACK, 'n':NEXT, 'q':QUIT"]

drawMusic :: UIState -> Widget n
drawMusic ui = withBorderStyle BS.unicode $ B.borderWithLabel (str "FFP Music Player") $ (C.center (drawAlbumCover ui) <+> B.vBorder <+> drawInfo ui)

drawInfo :: UIState -> Widget n
drawInfo ui = vBox [C.center (str "Title"), C.center (str "Song"), C.center (str "Artist"), C.center (str "Review")]

drawAlbumCover :: UIState -> Widget n
drawAlbumCover ui = do
  let image = ui ^. (appState . albumCover)
  B.border $ greedyRectangularImageWidget image

drawFunction =
  C.vLimit 3 $
    C.center $
      padRight (Pad 2) drawPrevious <+> padRight (Pad 2) drawStop <+> padRight (Pad 2) drawPlay <+> padRight (Pad 2) drawNext

-- padRight (Pad 2) drawPrevious <+> padRight (Pad 2) drawPause <+> padRight (Pad 2) withAttr $ playAttr . visible  <+> padRight (Pad 2) drawNext

drawPlay = withAttr playAttr $ str "Play"

drawStop = withAttr stopAttr $ str "Stop"

drawNext = withAttr nextAttr $ str "Next"

drawPrevious = withAttr previousAttr $ str "Previous"

drawSearch :: Bool -> Widget n
drawSearch b = case b of
  True -> vLimit 5 $ hBox []

-- drawSearch :: St -> Widget a
-- drawSearch st = C.hCenterLayer ( vLimit 3 $ hLimit 50 $ E.renderEditor  (str . unlines) True (st^_edit))

handleEvent :: UIState -> BrickEvent Name Event -> EventM Name (Next UIState)
handleEvent ui (AppEvent MarkAlbumCoverDirty) = do
  vty <- getVtyHandle
  liftIO $ refresh vty
  continue ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'p') [])) = exec CONTROLLER.play ui
-- handleEvent ui (VtyEvent (V.EvKey (V.KChar 's') [])) = pause a
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'f') [])) = search ui
-- handleEvent ui (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ step ui
-- handleEvent ui (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue $ step a
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'm') [V.MMeta])) = do
  -- easter egg, Key: Alt + M
  sendEvent MarkAlbumCoverDirty ui
  exec CONTROLLER.mandelbrot ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ui
handleEvent ui _ = continue ui

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

-- step :: AppState -> AppState
-- step a = a {_isPlaying = True}

-- pause :: AppState -> EventM Name (Next AppState)
-- pause a = do
--           a' <- liftIO $ execAppStateIO CONTROLLER.pause a
--           liftIO $ putStrLn $ show a'
--           continue a'

-- Instead of changing AppState, it should start the search function in controller
search :: UIState -> EventM Name (Next UIState)
search a = undefined

-- search a = do
--            liftIO $ putStrLn "Please enter a song name or artist name"
--            c <- getLine
--            a'<- execAppStateIO $ CONTROLLER.search c
--            continue a'
