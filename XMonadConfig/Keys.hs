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


-- | the thumb mask
thumbMask :: KeyMask
thumbMask = altMask

-- | the ring (finger) mask
ringMask :: KeyMask
ringMask = superMask

-- | the little (finger) mask
littleMask :: KeyMask
littleMask = controlMask


myToggleStrutsKey :: LayoutClass l Window => XConfig l -> (KeyMask, KeySym)
myToggleStrutsKey _ = (thumbMask .|. littleMask, xK_g)

myKeys :: Keys
myKeys _ = M.fromList $
  [ ((thumbMask .|. littleMask, xK_a), sinkAll)
  , ((thumbMask .|. littleMask, xK_c), kill)
  , ((thumbMask .|. littleMask, xK_h), windows swapUp)
  , ((thumbMask .|. littleMask, xK_i), nextScreen)
  , ((thumbMask .|. littleMask, xK_l), windows swapDown)
  , ((thumbMask .|. littleMask, xK_n), sendMessage NextLayout)
  , ((thumbMask .|. littleMask, xK_q), restartXMonadConfig)
  , ((thumbMask, xK_h), windows focusUp)
  , ((thumbMask, xK_j), withFocused $ sendMessage . MergeAll)
  , ((thumbMask, xK_k), withFocused $ sendMessage . UnMerge)
  , ((thumbMask, xK_l), windows focusDown)
  , ((ringMask, xK_F1), spawn "light -U 5")
  , ((ringMask, xK_F2), spawn "light -A 5")
  , ((ringMask, xK_F4), spawn "amixer -c 1 set Master toggle && amixer -c 1 set Speaker unmute")
  , ((ringMask, xK_F5), spawn "amixer -c 1 set Master 10-")
  , ((ringMask, xK_F6), spawn "amixer -c 1 set Master 10+")
  , ((ringMask, xK_F10), spawn "systemctl hibernate ; slock")
  , ((ringMask, xK_F11), spawn "systemctl suspend ; slock")
  , ((ringMask, xK_F12), spawn "slock")
  , ((ringMask, xK_c), withHomeDir $ spawn . (<> "/.xmonad/bin/trackpad-toggle.sh"))
  , ((ringMask, xK_e), spawn "thunar")
  , ((ringMask, xK_f), spawn myWebBrowser)
  , ((ringMask, xK_h), withFocused $ keysMoveWindow (-5,0))
  , ((ringMask, xK_j), withFocused $ keysMoveWindow (0,5))
  , ((ringMask, xK_k), withFocused $ keysMoveWindow (0,-5))
  , ((ringMask, xK_l), withFocused $ keysMoveWindow (5,0))
  , ((ringMask, xK_m), spawn "pavucontrol")
  , ((ringMask, xK_r), spawn "dmenu_run")
  , ((ringMask, xK_s), spawn "franz-bin")
  , ((ringMask, xK_t), spawn myTerminal)
  -- Another KeyMask
  , ((noModMask, xK_F1), setKeyLayout USKeyboardLayout) -- I never used F1 key in anywhere
  , ((shiftMask, xK_F1), setKeyLayout ResetSetXKBMAP)
  , ((noModMask, xK_F2), setKeyLayout JPKeyboardLayout) -- I never used F2 key in anywhere
  , ((shiftMask, xK_F2), withHomeDir $ spawn . (<> "/bin/dunst-swap-screen.sh"))
  , ((shiftMask, xK_F3), withHomeDir $ spawn . (<> "/bin/dzen2statusbar.sh"))
  , ((noModMask, xK_Print), takeScreenShot ActiveWindow)
  , ((shiftMask, xK_Print), takeScreenShot FullScreen)
  ]
  -- Switch a workspace
  ++ [ ((thumbMask .|. littleMask, numKey), windows $ greedyView workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
     ]
  -- Move a current window to a worskpace
  ++ [((ringMask, numKey), windows $ shift workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
     ]
