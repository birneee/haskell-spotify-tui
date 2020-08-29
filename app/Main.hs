{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Controller as CONTROLLER (play)
import Tui.Tui as TUI (app, playAttr, stopAttr, nextAttr, previousAttr)
import AppState (AppState, execAppStateIO, newAppState)

import Brick
  (App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , defaultMain
  )


main :: IO AppState
main = do
       defaultMain app newAppState