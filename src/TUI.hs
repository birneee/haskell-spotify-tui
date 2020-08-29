{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (tuiMain) where

import qualified Controller as CONTROLLER (play)
import AppState (AppState, execAppStateIO, newAppState)
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
-- Editor
data St =
    St {_edit :: E.Editor String Name1
       }

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (playAttr,      V.white `on` V.green),
    (stopAttr,      V.white `on` V.red),
    (nextAttr,      V.white `on` V.blue),
    (previousAttr,      V.white `on` V.cyan)
    ]

playAttr, stopAttr, nextAttr, previousAttr :: AttrName
playAttr = "playAttr"
stopAttr = "stopAttr"
nextAttr = "nextAttr"
previousAttr = "previousAttr"

app :: App AppState Tick Name
app = App { appDraw = drawUI 
          , appChooseCursor = M.showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

tuiMain :: IO ()
tuiMain = void $ defaultMain app newAppState
      

drawUI :: AppState -> [Widget Name]
drawUI a =  [C.center $ drawMain]

drawMain = vLimit 100 $ vBox [drawMusic  <=> C.center (drawFunction), str $ "'p':PLAY, 's':STOP, 'p':BACK, 'n':NEXT"]

drawMusic :: Widget a
drawMusic = withBorderStyle BS.unicode $ B.borderWithLabel (str "FFP Music Player") $ (C.center drawIcon <+> B.vBorder <+> drawInfo)

drawInfo = vBox [  C.center (str"Title"),  C.center (str"Song"),  C.center(str"Artist"), C.center(str"Review")]

drawIcon::Widget a
drawIcon = C.withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Album") (str "")

drawFunction = padRight (Pad 2) drawPrevious <+> padRight (Pad 2) drawStop <+> padRight (Pad 2) drawPlay <+> padRight (Pad 2) drawNext
-- padRight (Pad 2) drawPrevious <+> padRight (Pad 2) drawPause <+> padRight (Pad 2) withAttr $ playAttr . visible  <+> padRight (Pad 2) drawNext

drawPlay = withAttr playAttr $ str "Play"

drawStop = withAttr stopAttr $ str "Stop"

drawNext = withAttr nextAttr $ str "Next"

drawPrevious = withAttr previousAttr $ str "Previous"

-- drawSearch :: St -> Widget a
-- drawSearch st = C.hCenterLayer ( vLimit 3 $ hLimit 50 $ E.renderEditor  (str . unlines) True (st^_edit))

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent a (VtyEvent (V.EvKey (V.KChar 'p') [])) = play a 
-- handleEvent a (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ step a 
-- handleEvent a (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ step a 
-- handleEvent a (VtyEvent (V.EvKey (V.KChar 'n') [])) = continue $ step a 
handleEvent a _ = continue a

-- step :: AppState -> AppState
-- step a = a {_isPlaying = True}

play :: AppState -> EventM Name (Next AppState)
play a = do 
         a' <- liftIO $ execAppStateIO CONTROLLER.play a 
        --  let old = a ^. isPlaying
         liftIO $ putStrLn $ show a' 
         continue a'

          
-- exec :: AppState ->  EventM Name (Next AppState)