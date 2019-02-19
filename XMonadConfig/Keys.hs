-- |
-- Exposes 'myKeys' for 'XConfig.keys'.
--
-- This module exports compile time processings
-- for many keyboard layouts (e.g. HHKB Lite2 US, Surface type cover, HHKB Lite2 JP + HHKB Lite2 US)
module XMonadConfig.Keys where

import Data.Map.Lazy (Map)
import Data.Semigroup ((<>))
import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.FloatKeys (keysMoveWindow)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Layout (ChangeLayout (..))
import XMonad.Layout.SubLayouts (GroupMsg (..))
import XMonad.Operations (sendMessage, withFocused)
import XMonad.StackSet (focusDown, focusUp, greedyView, shift, swapDown, swapUp)
import XMonadConfig.Keys.FingersMask (currentFingers, fromFingersMask)
import XMonadConfig.Keys.Menus
import XMonadConfig.XConfig (myTerminal, myWebBrowser, myWorkspaces)
import qualified Data.Map.Lazy as M

type Keys = XConfig Layout -> Map (KeyMask, KeySym) (X ())

myToggleStrutsKey :: XConfig l -> (KeyMask, KeySym)
myToggleStrutsKey _ =
  let (_, littleMask, thumbMask) = fromFingersMask currentFingers
  in (thumbMask .|. littleMask, xK_g)

myKeys :: Keys
myKeys _ =
  let (ringMask, littleMask, thumbMask) = fromFingersMask currentFingers
  in M.fromList $
    [ ((thumbMask .|. littleMask, xK_a), sinkAll)
    , ((thumbMask .|. littleMask, xK_c), kill)
    , ((thumbMask .|. littleMask, xK_d), menusMenu)
    , ((thumbMask .|. littleMask, xK_f), spawn "xfce4-find-cursor")
    , ((thumbMask .|. littleMask, xK_h), windows swapUp)
    , ((thumbMask .|. littleMask, xK_i), nextScreen)
    , ((thumbMask .|. littleMask, xK_k), fingerLayoutMenu)
    , ((thumbMask .|. littleMask, xK_l), windows swapDown)
    , ((thumbMask .|. littleMask, xK_m), xmodmapMenu)
    , ((thumbMask .|. littleMask, xK_n), sendMessage NextLayout)
    , ((thumbMask .|. littleMask, xK_r), recompileMenu)
    , ((ringMask .|. littleMask, xK_s), spawn "amixer -c 1 set Master 10-") -- thumb+ring keys adjusts for MISTEL Barroco MD-600
    , ((ringMask .|. littleMask, xK_d), spawn "amixer -c 1 set Master 10+")
    , ((ringMask .|. littleMask, xK_f), spawn "amixer -c 1 set Master toggle && amixer -c 1 set Speaker unmute")
    --, ((ringMask .|. littleMask, xK_h), spawn "xdotool key --window $(xdotool getactivewindow) Left")
    --, ((ringMask .|. littleMask, xK_j), spawn "xdotool key --window $(xdotool getactivewindow) Down")
    --, ((ringMask .|. littleMask, xK_k), spawn "xdotool key --window $(xdotool getactivewindow) Up")
    --, ((ringMask .|. littleMask, xK_l), spawn "xdotool key --window $(xdotool getactivewindow) Right")
    , ((thumbMask, xK_h), windows focusUp)
    , ((thumbMask, xK_j), withFocused $ sendMessage . MergeAll)
    , ((thumbMask, xK_k), withFocused $ sendMessage . UnMerge)
    , ((thumbMask, xK_l), windows focusDown)
    , ((ringMask, xK_F1), spawn "light -U 3")
    , ((ringMask, xK_F2), spawn "light -A 3")
    , ((ringMask, xK_F4), spawn "amixer -c 1 set Master toggle && amixer -c 1 set Speaker unmute")
    , ((ringMask, xK_F5), spawn "amixer -c 1 set Master 10-")
    , ((ringMask, xK_F6), spawn "amixer -c 1 set Master 10+")
    , ((ringMask, xK_F10), spawn "systemctl hibernate ; slock")
    , ((ringMask, xK_F11), spawn "systemctl suspend ; slock")
    , ((ringMask, xK_F12), spawn "slock")
    , ((ringMask, xK_c), withHomeDir $ spawn . (<> "/.xmonad/bin/trackpad-toggle.sh"))
    , ((ringMask, xK_e), spawn "thunar")
    , ((ringMask, xK_f), spawn myWebBrowser)
    , ((ringMask, xK_h), withFocused $ keysMoveWindow (-5, 0))
    , ((ringMask, xK_j), withFocused $ keysMoveWindow (0, 5))
    , ((ringMask, xK_k), withFocused $ keysMoveWindow (0, -5))
    , ((ringMask, xK_l), withFocused $ keysMoveWindow (5, 0))
    , ((ringMask, xK_m), spawn "pavucontrol")
    , ((ringMask, xK_r), spawn "dmenu_run")
    , ((ringMask, xK_t), spawn myTerminal)
    -- Another KeyMask
    , ((shiftMask, xK_F1), xmodmapMenu)
    , ((shiftMask, xK_F2), withHomeDir $ spawn . (<> "/bin/dunst-swap-screen.sh"))
    , ((shiftMask, xK_F3), withHomeDir $ spawn . (<> "/bin/dzen2statusbar.sh"))
    , ((noModMask, xK_Print), takeScreenShot ActiveWindow)
    , ((shiftMask, xK_Print), takeScreenShot FullScreen)
    ] <> -- Switch workspaces
    [ ((thumbMask .|. littleMask, numKey), windows $ greedyView workspace)
    | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
    ] <> -- Move a current window to a worskpace
    [ ((ringMask, numKey), windows $ shift workspace)
    | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
    ]
