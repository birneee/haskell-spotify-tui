module Main where

<<<<<<< HEAD
<<<<<<< HEAD
=======
import qualified Controller as CONTROLLER (play)
import Tui.Tui as TUI (app, playAttr, stopAttr, nextAttr, previousAttr)
import AppState (AppState, execAppStateIO, newAppState)

>>>>>>> tui
import Brick
  (App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , defaultMain
  )


<<<<<<< HEAD
-- appHandleEvent :: s -> BrickEvent n e -> EventM n (Next s)
-- handleEvent :: Song -> BrickEvent Name e -> EventM Name (Next Song)
-- handleEvent s (VtyEvent ev) = case ev of
--   V.EvKey ()
handleEvent =undefined
action :: Song -> Song
action s = s
=======
main :: IO AppState
main = do
       defaultMain app newAppState
>>>>>>> tui
=======
import           CLI (cliMain)


main :: IO ()
main = cliMain
>>>>>>> e97b46259dfd6aafc17e2579dd6c3aba314411d5
