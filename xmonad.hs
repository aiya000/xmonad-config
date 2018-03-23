{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad ((>=>))
import Data.Monoid (All)
import Data.Semigroup ((<>))
import Data.String.Here (i)
import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (statusBar, dzenPP)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks (AvoidStruts)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.EZConfig (additionalMouseBindings)
import XMonadConfig.Keys (myKeys, superMask, altMask)
import XMonadConfig.LayoutHook (myLayoutHook)
import XMonadConfig.XConfig (myTerminal, myWorkspaces)

main :: IO ()
main =
  dzen >=> xmonad $ desktopConfig
    { terminal           = myTerminal
    , modMask            = superMask
    , keys               = myKeys
    , borderWidth        = 2
    , layoutHook         = myLayoutHook
    , startupHook        = myStartupHook
    , manageHook         = myManageHook
    , workspaces         = myWorkspaces
    , focusFollowsMouse  = False
    , focusedBorderColor = "#0000ff"
    , handleEventHook    = myHandleEventHook
    }
    `additionalMouseBindings` myMouseBindings
  where
    -- Similar to 'XMonad.Hooks.DynamicLog',
    -- but avoid https://github.com/xmonad/xmonad/issues/79 by -dock,
    -- with my preference
    dzen :: LayoutClass l Window => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
    dzen = statusBar ("dzen2 " <> dzenOptions) dzenPP (const (altMask .|. controlMask, xK_g))

    dzenOptions :: String
    dzenOptions =
      let surfacePro3ScreenWidth = 2160
      in [i|-dock -w ${show surfacePro3ScreenWidth}|]


myStartupHook :: X ()
myStartupHook =
  setWMName "LG3D" -- Fix to start of Java Swing apps

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Xfce4-panel" --> doIgnore
  , className =? "Xfdesktop"   --> doIgnore
  , className =? "io.github.aiya000.DoromochiApp" --> doFloat
  ]

myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
  [ ((altMask .|. controlMask, button1), mouseResizeWindow)
  , ((shiftMask .|. controlMask, button1), mouseMoveWindow)
  ]

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def <+> fullscreenEventHook
