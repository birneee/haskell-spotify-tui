{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

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
