{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- For 'keys'.
--
-- Please see 'myKeys' (the keymappings for my casual keyboards)
-- and 'myUnixKeys' (for HHKB like keyboards)
module XMonadConfig.Keys
  ( Keys
  , myKeys
  , altMask
  , superMask
  , myToggleStrutsKey
  ) where

import Data.Map.Lazy (Map)
import Data.Semigroup ((<>))
import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.FloatKeys (keysMoveWindow)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Layout (ChangeLayout(..))
import XMonad.Layout.SubLayouts (GroupMsg(..))
import XMonad.Operations (sendMessage, withFocused)
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown, greedyView, shift)
import XMonadConfig.CommandWrapper
import XMonadConfig.XConfig (myTerminal, myWebBrowser, myWorkspaces)
import qualified Data.Map.Lazy as M

type Keys = XConfig Layout -> Map (KeyMask, KeySym) (X ())


altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask


myToggleStrutsKey :: LayoutClass l Window => XConfig l -> (KeyMask, KeySym)
myToggleStrutsKey _ = (altMask .|. controlMask, xK_g)

myKeys :: Keys
myKeys _ = M.fromList $
  [ ((altMask .|. controlMask, xK_a), sinkAll)
  , ((altMask .|. controlMask, xK_c), kill)
  , ((altMask .|. controlMask, xK_h), windows swapUp)
  , ((altMask .|. controlMask, xK_i), nextScreen)
  , ((altMask .|. controlMask, xK_l), windows swapDown)
  , ((altMask .|. controlMask, xK_n), sendMessage NextLayout)
  , ((altMask .|. controlMask, xK_q), restartXMonadConfig)
  , ((altMask, xK_h), windows focusUp)
  , ((altMask, xK_j), withFocused $ sendMessage . MergeAll)
  , ((altMask, xK_k), withFocused $ sendMessage . UnMerge)
  , ((altMask, xK_l), windows focusDown)
  , ((superMask, xK_F1), spawn "light -U 5")
  , ((superMask, xK_F2), spawn "light -A 5")
  , ((superMask, xK_F4), spawn "amixer -c 1 set Master toggle && amixer -c 1 set Speaker unmute")
  , ((superMask, xK_F5), spawn "amixer -c 1 set Master 3-")
  , ((superMask, xK_F6), spawn "amixer -c 1 set Master 3+")
  , ((superMask, xK_F10), lockScreen)
  , ((superMask, xK_F11), lockScreenSuspend)
  , ((superMask, xK_F12), lockScreenHibernate)
  , ((superMask, xK_c), withHomeDir $ spawn . (<> "/.xmonad/bin/trackpad-toggle.sh"))
  , ((superMask, xK_e), spawn "thunar")
  , ((superMask, xK_f), spawn myWebBrowser)
  , ((superMask, xK_h), withFocused $ keysMoveWindow (-5,0))
  , ((superMask, xK_j), withFocused $ keysMoveWindow (0,5))
  , ((superMask, xK_k), withFocused $ keysMoveWindow (0,-5))
  , ((superMask, xK_l), withFocused $ keysMoveWindow (5,0))
  , ((superMask, xK_m), spawn "pavucontrol")
  , ((superMask, xK_r), spawn "dmenu_run")
  , ((superMask, xK_s), spawn "franz-bin")
  , ((superMask, xK_t), spawn myTerminal)
  -- Another KeyMask
  , ((noModMask, xK_F1), resetXKeyboardLayout USKeyboardLayout) -- I never used F1 key in anywhere
  , ((shiftMask, xK_F1), resetXKeyboardLayout ResetSetXKBMAP)
  , ((shiftMask, xK_F2), withHomeDir $ spawn . (<> "/bin/dunst-swap-screen.sh"))
  , ((noModMask, xK_Print), takeScreenShot ActiveWindow)
  , ((shiftMask, xK_Print), takeScreenShot FullScreen)
  ]
  -- Switch workspace
  ++ [ ((altMask .|. controlMask, numKey), windows $ greedyView workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
     ]
  -- Move current window to target worskpace
  ++ [((superMask, numKey), windows $ shift workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
     ]
