{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (tuiMain) where
import Control.Lens
import qualified Controller as CONTROLLER ( initAppState, togglePlay, search)
import AppState (searchInput, showSearch, AppState, execAppStateIO)
import Data.Char
import Control.Lens
import Brick
  (App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  , defaultMain
  ,resizeOrQuit
  ,attrName
  )

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import Brick.AttrMap (AttrMap, AttrName, attrMap)

import Brick.Types
  ( Widget
  , Padding(..)
  )

import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , withAttr
  , vLimit
  , hLimit
  , hBox
  , updateAttrMap
  , withBorderStyle
  , txt
  , str
  , visible
  )
  
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Core as C
import qualified Brick.Types as T

import qualified Brick.Focus as F
import Brick.Types (Extent)
import Brick.Types (Location)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (void)

data State = String
data Event = Event
data Tick = Tick
type Name = ()

data Name1 = TextBox
          deriving (Show, Ord, Eq)
data UIState =
    UIState {_edit :: E.Editor String Name, -- Search input
             _appState :: AppState
       }

makeLenses ''UIState


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (playAttr,      V.white `on` V.green),
    (stopAttr,      V.white `on` V.red),
    (nextAttr,      V.white `on` V.blue),
    (previousAttr,      V.white `on` V.cyan),
    (E.editAttr, V.white `on` V.black)
    ]

playAttr, stopAttr, nextAttr, previousAttr :: AttrName
playAttr = "playAttr"
stopAttr = "stopAttr"
nextAttr = "nextAttr"
previousAttr = "previousAttr"

app :: App UIState () Name
app = App { appDraw = drawUI
          , appChooseCursor = M.showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
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
drawUI ui =  [C.center $ drawMain ui]

drawMain  ui= vLimit 100 $ vBox [drawMusic  <=> C.center (drawFunction) <=> drawSearch ui]

drawMusic :: Widget a
drawMusic = withBorderStyle BS.unicode $ B.borderWithLabel (str "FFP Music Player") $ (C.center drawIcon <+> B.vBorder <+> drawInfo)

drawInfo = vBox [  C.center (str"Title"),  C.center (str"Song"),  C.center(str"Artist"), C.center(str"Review")]

drawIcon:: Widget a
drawIcon = C.withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Album") (str "")

drawFunction = padRight (Pad 2) drawPrevious <+> padRight (Pad 2) drawStop <+> padRight (Pad 2) drawPlay <+> padRight (Pad 2) drawNext

drawSearch :: UIState -> Widget Name
drawSearch st = str "Input " <+> (vLimit 1 $ E.renderEditor (str . unlines) True (st^.edit))

drawPlay = withAttr playAttr $ str "Play"

drawStop = withAttr stopAttr $ str "Stop"

drawNext = withAttr nextAttr $ str "Next"

drawPrevious = withAttr previousAttr $ str "Previous"

handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)
handleEvent a (VtyEvent (V.EvKey (V.KEsc) [])) = halt a                       
handleEvent ui (VtyEvent (V.EvKey V.KEnter [])) | ui^.appState^.showSearch = do
                                                                             let content = head $ E.getEditContents $ ui^.edit
                                                                             liftIO $ putStrLn content
                                                                             let ui' =  ui & (appState . searchInput ) .~ ""
                                                                             search ui'
                                                                               
                                                                            --  (ui . appState . searchInput) .= content --setter
                                                                             -- continue ui
                                                                             
handleEvent ui (VtyEvent ev) | ui^.appState^.showSearch = continue =<< T.handleEventLensed ui edit E.handleEditorEvent ev -- for typing input
handleEvent ui (VtyEvent (V.EvKey (V.KChar ' ') []))  = play ui --leer Taste togglePlay
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'n') [])) = next ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'b') [])) = previous ui
handleEvent ui _ = continue ui

-- Instead of changing AppState, it should start the search function in controller
search :: UIState-> EventM Name (Next UIState)
search ui = let a = ui ^. appState
                u = execAppStateIO CONTROLLER.search a
                ui.appState = u
                in continue ui

play :: UIState-> EventM Name (Next UIState)
play ui = let a = ui ^. appState
              u = execAppStateIO CONTROLLER.togglePlay a
              ui.appState = u
              in continue ui

next :: UIState-> EventM Name (Next UIState)
next = undefined

previous :: UIState-> EventM Name (Next UIState)
previous = undefined
 