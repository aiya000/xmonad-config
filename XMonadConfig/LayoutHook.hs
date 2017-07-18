{-# LANGUAGE TypeOperators #-}

module XMonadConfig.LayoutHook
  ( myLayoutHook
  ) where

import XMonad (Window)
import XMonad.Layout ((|||), Choose, Full(..))
import XMonad.Layout.Decoration (Decoration, DefaultShrinker)
import XMonad.Layout.Gaps (Gaps, gaps)
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Simplest (Simplest)
import XMonad.Layout.SubLayouts (subTabbed, Sublayout)
import XMonad.Layout.Tabbed (TabbedDecoration)
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Util.Types (Direction2D(..))


infixr 1 :$
type (:$) = ModifiedLayout

type (:.) x y z = x :$ (y :$ z)

infixr 2 :|||
type (:|||) = Choose


type MyLayoutHook  = (TaskbarMargin :$ TwoTabbedPane :||| Grid) :||| Full
type TaskbarMargin = Gaps
type TwoTabbedPane = SubTabbed TwoPane
type SubTabbed x   = (Decoration TabbedDecoration DefaultShrinker :. Sublayout Simplest) x


myLayoutHook :: MyLayoutHook Window
myLayoutHook = (taskbarMargin $ twoTabbedPane ||| Grid) ||| Full
  where
    taskbarMargin = gaps [(D, 40)]
    twoTabbedPane = subTabbed $ TwoPane (1/55) (1/2)
