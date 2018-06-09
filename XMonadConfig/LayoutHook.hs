{-# LANGUAGE TypeOperators #-}

module XMonadConfig.LayoutHook
  ( myLayoutHook
  ) where

import XMonad (Window)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts)
import XMonad.Layout ((|||), Choose, Full(..))
import XMonad.Layout.Decoration (Decoration, DefaultShrinker)
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Simplest (Simplest)
import XMonad.Layout.StackTile (StackTile(..))
import XMonad.Layout.SubLayouts (subTabbed, Sublayout)
import XMonad.Layout.Tabbed (TabbedDecoration)
import XMonad.Layout.TwoPane (TwoPane(..))


infixr 1 :$
type (:$) = ModifiedLayout

type (:.) x y z = x :$ (y :$ z)

infixr 2 :|||
type (:|||) = Choose


type MyLayoutHook  = AvoidStruts :$ TwoTabbedPane :||| StackTile :||| Grid :||| Full
type TwoTabbedPane = SubTabbed TwoPane
type SubTabbed x   = (Decoration TabbedDecoration DefaultShrinker :. Sublayout Simplest) x


myLayoutHook :: MyLayoutHook Window
myLayoutHook = avoidStruts $ twoTabbedPane ||| StackTile 1 (3/200) (1/2) ||| Grid ||| Full
  where
    twoTabbedPane = subTabbed $ TwoPane (1/55) (1/2)
