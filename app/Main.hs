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
  , withAttr
  , vLimit
  , hLimit
  , hBox
  , updateAttrMap
  , withBorderStyle
  , txt
  , str
  )
import Brick.AttrMap (attrMap, AttrMap)
import qualified Brick.Widgets.Core as C

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
-- drawUI _ = [withBorderStyle BS.unicode $ C.hCenter $ hLimit 100 $ vLimit 200 $ str("ABC")] 
-- [ui] where ui = C.hCenter $ vLimit 15 $ hLimit 150 $ B.borderWithLabel (str "Label") $ drawMenu
drawUI _ =  [C.center $ drawMain]

--theMap = undefined

-- drawMenu = vBox [ drawPlay, padTop (Pad 1) $ drawStop, padTop (Pad 1) $ drawPause]
drawMain = vLimit 100 $ vBox [drawMusic <=> B.hBorder <=> drawFunction <=> drawSearch]

drawMusic :: Widget a
drawMusic= vBox [  drawIcon, padLeft (Pad 20) $ str"Title", padLeft (Pad 20) $ str"Song", padLeft (Pad 20) $ str"Artist", padLeft (Pad 20) $ str"Review"]
  -- withBorderStyle (hSize) BS.unicodeBold $ B.borderWithLabel (str "FFP-Music-Player") $ C.hCenter $ padAll 1 $ str("")

drawIcon::Widget a
drawIcon = C.withBorderStyle BS.unicodeBold $ B.vBorder

drawFunction = hLimit 50 $ drawPrevious

drawPlay = str "Play"

drawStop = str "Stop"

drawPause = str "Pause"

drawNext = str "Next"

drawPrevious = str "Previous"

drawSearch = str "Search"
-- appHandleEvent :: s -> BrickEvent n e -> EventM n (Next s)
-- handleEvent :: Song -> BrickEvent Name e -> EventM Name (Next Song)
-- handleEvent s (VtyEvent ev) = case ev of
--   V.EvKey ()
handleEvent = undefined
