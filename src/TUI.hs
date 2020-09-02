{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (tuiMain) where

import ApiObjects.Track (trackId)
import AppState (AppState, execAppStateIO, isPlaying, searchInput, searchResults, showSearch)
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
import Brick.Types (Extent, Location, Padding (..), Widget)
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
    visible,
    withAttr,
    withBorderStyle,
    (<+>),
    (<=>),
  )
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Controller as CONTROLLER (initAppState, next, pause, play, previous, search)
import Data.Char
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

data State = String

data Event = Event

data Tick = Tick

type Name = ()

-- data Name1 = TextBox
--   deriving (Show, Ord, Eq)

data UIState = UIState
  { _edit :: E.Editor String Name, -- Search input
    _appState :: AppState
  }

makeLenses ''UIState

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (playAttr, V.white `on` V.green),
      (stopAttr, V.white `on` V.red),
      (nextAttr, V.white `on` V.blue),
      (previousAttr, V.white `on` V.cyan),
      (E.editAttr, V.white `on` V.black)
    ]

playAttr, stopAttr, nextAttr, previousAttr :: AttrName
playAttr = "playAttr"
stopAttr = "stopAttr"
nextAttr = "nextAttr"
previousAttr = "previousAttr"

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
  return $ UIState (E.editor () Nothing "") appState

drawUI :: UIState -> [Widget Name]
drawUI ui = [C.center $ drawMain ui]

drawMain ui = vLimit 100 $ vBox [drawMusic ui <=> C.center (drawFunction) <=> drawSearch ui]

drawMusic :: UIState -> Widget a
drawMusic ui = withBorderStyle BS.unicode $ B.borderWithLabel (str "FFP Music Player") $ (C.center drawIcon <+> B.vBorder <+> drawInfo ui)

-- drawInfo = vBox [  C.center (str"Title"),  C.center (str"Song"),  C.center(str"Artist"), C.center(str"Review")]
drawInfo :: UIState -> Widget a
drawInfo ui
  | ui ^. appState ^. showSearch = defaultUI
  | length (ui ^. appState ^. searchResults) > 0 =
    let id = ui ^. appState ^. searchResults ^. trackId
     in B.borderWithLabel (str "Result") (L.renderList listDrawElement False (L.list "list" (Vec.fromList $ id) 1))

-- (L.renderList listDrawElement False (L.list () $ Vec.fromList $ ui ^. appState ^. searchResults ^. trackId))
defaultUI :: Widget a
defaultUI = C.withBorderStyle BS.unicode $ str " "

drawIcon :: Widget a
drawIcon = C.withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Album") (str "")

drawFunction = padRight (Pad 2) drawPrevious <+> padRight (Pad 2) drawStop <+> padRight (Pad 2) drawPlay <+> padRight (Pad 2) drawNext

drawSearch :: UIState -> Widget Name
drawSearch st = str "Input " <+> (vLimit 1 $ E.renderEditor (str . unlines) True (st ^. edit))

drawPlay = withAttr playAttr $ str "Play"

drawStop = withAttr stopAttr $ str "Stop"

drawNext = withAttr nextAttr $ str "Next"

drawPrevious = withAttr previousAttr $ str "Previous"

handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)
handleEvent a (VtyEvent (V.EvKey (V.KEsc) [])) = halt a
handleEvent ui (VtyEvent (V.EvKey V.KEnter [])) | ui ^. appState ^. showSearch = do
  let content = head $ E.getEditContents $ ui ^. edit
  liftIO $ putStrLn content
  let ui' = ui & (appState . searchInput) .~ ""
  setPlayerModus "search" ui'
handleEvent ui (VtyEvent ev) | ui ^. appState ^. showSearch = continue =<< T.handleEventLensed ui edit E.handleEditorEvent ev -- for typing input
handleEvent ui (VtyEvent (V.EvKey (V.KChar ' ') []))
  | ui ^. appState ^. isPlaying = setPlayerModus "play" ui
  | otherwise = setPlayerModus "pause" ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'n') [])) = setPlayerModus "next" ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'b') [])) = setPlayerModus "previous" ui
handleEvent ui _ = continue ui

listDrawElement b a = str a

setPlayerModus :: String -> UIState -> EventM Name (Next UIState)
setPlayerModus "play" ui =
  let a = ui ^. appState
      u = execAppStateIO CONTROLLER.play a
      ui . appState = u
   in continue ui
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