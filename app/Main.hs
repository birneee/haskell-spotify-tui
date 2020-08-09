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

-- ui :: Widget ()
-- ui = str "Hello, world!"
data State = String
data Event = Event
type Name = ()

-- ui = str "Hello, world!"


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (attrName "keyword1",      fg V.magenta)
    , (attrName "keyword2",      V.white `on` V.blue)
    ]

-- app :: App State Event Name
app :: App () e Name
app = App { appDraw = drawUI --appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = resizeOrQuit --handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = defaultMain app ()

--handleEvent :: State -> BrickEvent Event Name
--handleEvent = undefined

-- drawUI :: String -> [Widget String]
drawUI :: () -> [Widget a]
drawUI _ =  [vBox [ drawTest, drawTest]]

drawTest = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show 1

--theMap = undefined