{-# LANGUAGE FlexibleContexts #-}

import Control.Monad ((>=>))
import Data.Monoid (All)
import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (statusBar, dzenPP)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks (AvoidStruts)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.EZConfig (additionalMouseBindings)
import XMonadConfig.Keys (myToggleStrutsKey, myKeys, superMask, altMask)
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
    -- Run dummy dzen2 for 'myToggleStrutsKey', please see ~/.zshrc_env for real dzen2 startup
    dzen :: LayoutClass l Window => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
    dzen = statusBar "echo 'hi' | dzen2 -dock " dzenPP myToggleStrutsKey


myStartupHook :: X ()
myStartupHook =
  setWMName "LG3D" -- Fix startings of Java Swing apps

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
