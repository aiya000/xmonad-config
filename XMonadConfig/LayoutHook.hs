{-# LANGUAGE TypeOperators #-}

module XMonadConfig.LayoutHook
  ( myLayoutHook
  ) where

import XMonad (Window, Mirror(..))
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts)
import XMonad.Layout (Choose, Full (..), (|||))
import XMonad.Layout.Decoration (Decoration, DefaultShrinker)
import XMonad.Layout.Grid (Grid (..))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Simplest (Simplest)
import XMonad.Layout.StackTile (StackTile (..))
import XMonad.Layout.SubLayouts (Sublayout, subTabbed)
import XMonad.Layout.Tabbed (TabbedDecoration)
import XMonad.Layout.TwoPane (TwoPane (..))

myLayoutHook :: ModifiedLayout
                        AvoidStruts
                        (Choose
                           (ModifiedLayout
                              (Decoration TabbedDecoration DefaultShrinker)
                              (ModifiedLayout (Sublayout Simplest) TwoPane))
                           (Choose StackTile (Choose Grid (Choose (Mirror StackTile) Full))))
                        Window
myLayoutHook = avoidStruts $
  twoTabbedPane ||| stack ||| Grid ||| gimp ||| Full
  where
    nanika = 3 / 200

    stack = StackTile 1 nanika (1 / 2)
    twoTabbedPane = subTabbed $ TwoPane (1 / 55) (1 / 2)
    gimp = Mirror $ StackTile 1 nanika (1 / 6)
