{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

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
  )
  
import qualified Brick.Widgets.Edit as E
import Brick.AttrMap (attrMap, AttrMap)
import qualified Brick.Widgets.Core as C
import Brick.Types (Extent)
import Brick.Types (Location)

data State = String
data Event = Event
data Tick = Tick
-- type Name = ()

data Name = Info | Button1 | Button2 | Button3 | Prose | TextBox
          deriving (Show, Ord, Eq)

data St =
    St { _clicked :: [Extent Name]
       , _lastReportedClick :: Maybe (Name, Location)
       , _prose :: String
       , _edit :: E.Editor String Name
       }

-- State
-- data AppState = AppState {
--     _accessToken :: Maybe AccessToken,
--     _isPlaying   :: Bool
-- } deriving(Show, Eq)

-- $(makeLenses ''AppState)

-- newAppState :: AppState
-- newAppState = AppState {
--     _accessToken = Nothing,
--     _isPlaying = False
-- }

-- type AppStateT = StateT AppState
-- type AppStateM a = forall m. (Monad m) => AppStateT m a
-- type AppStateIO a = AppStateT IO a


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (attrName "keyword1",      fg V.magenta)
    , (attrName "keyword2",      V.white `on` V.blue)
    ]

-- app :: App State Event Name
app :: App () e Name
app = App { appDraw = drawUI --appDraw = drawUI
          , appChooseCursor = M.showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = defaultMain app ()

-- drawUI :: String -> [Widget String]
drawUI :: () -> [Widget a]
drawUI _ =  [C.center $ drawMain]

--theMap = undefined

-- drawMenu = vBox [ drawPlay, padTop (Pad 1) $ drawStop, padTop (Pad 1) $ drawPause]

drawMain = vLimit 100 $ vBox [drawMusic <=> B.hBorder <=> C.center (drawFunction)]

drawMusic :: Widget a
drawMusic = withBorderStyle BS.unicode $ B.borderWithLabel (str "FFP Music Player") $ (C.center drawIcon <+> B.vBorder <+> drawInfo)

drawInfo = vBox [  C.center (str"Title"),  C.center (str"Song"),  C.center(str"Artist"), C.center(str"Review")]

drawIcon::Widget a
drawIcon = C.withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Album") (str "")

drawFunction = padRight (Pad 2) drawPrevious <+> padRight (Pad 2) drawPause <+> padRight (Pad 2) drawPlay <+> padRight (Pad 2) drawNext

drawPlay = str "Play"

drawStop = str "Stop"

drawPause = str "Pause"

drawNext = str "Next"

drawPrevious = str "Previous"

-- drawSearch :: St -> Widget a
-- drawSearch st = C.hCenterLayer ( vLimit 3 $ hLimit 50 $ E.renderEditor  (str . unlines) True (st^_edit))

handleEvent = undefined
