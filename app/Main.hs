module Main where
import qualified Graphics.Vty as V

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
  , padLeft
  )
import Brick.Util (on, fg)
import Brick.Markup (markup, (@?))
import Brick.AttrMap (attrMap, AttrMap)
import Data.Text.Markup ((@@))

data State = String
data Event = Event
data Tick = Tick
type Name = ()

data Song = Song {title::String, length::Double, playing::Bool, inList:: Bool} deriving Show
-- song1 = Song { "123",100, False, False }

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
drawUI _ =  [C.vCenter $ vLimit 25 $ hLimit 150 $ B.borderWithLabel label $ (drawMenu)]
            where label = str "FFP Musik-Player"


--theMap = undefined

drawIcon = withBorderStyle BS.unicodeBold $ B.border $ str "Icon"
drawMenu = vBox [ drawPlay, padTop (Pad 1) $ drawStop, padTop (Pad 1) $ drawPause]

drawPlay = str "Play"

drawStop = str "Stop"

drawPause = str "Pause"

-- appHandleEvent :: s -> BrickEvent n e -> EventM n (Next s)
-- handleEvent :: Song -> BrickEvent Name e -> EventM Name (Next Song)
-- handleEvent s (VtyEvent ev) = case ev of
--   V.EvKey ()
handleEvent =undefined
action :: Song -> Song
action s = s