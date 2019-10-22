{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE TypeOperators #-}

module XMonadConfig.LayoutHook
  ( myLayoutHook
  ) where

import XMonad (Window)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout (Full (..), (|||))
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.TwoPane (TwoPane (..))

myLayoutHook :: _ Window
myLayoutHook = avoidStruts $
  TwoPane (1 / 55) (1 / 2) |||
  Full |||
  Grid
