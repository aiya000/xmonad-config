-- |
-- Exposes 'myKeys' for 'XConfig.keys'.
--
-- This module exports compile time processings
-- for many keyboard layouts (e.g. HHKB Lite2 US, Surface type cover, HHKB Lite2 JP + HHKB Lite2 US)
module XMonadConfig.Keys where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
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

type Keys = XConfig Layout -> Map (KeyMask, KeySym) (X ())

myToggleStrutsKey :: XConfig l -> (KeyMask, KeySym)
myToggleStrutsKey _ =
  let (_, littleMask, thumbMask) = fromFingersMask currentFingers
  in (thumbMask .|. littleMask, xK_g)

myKeys :: Keys
myKeys _ = M.fromList $
  keys <>
  switchingWorkspaces <>
  puttingWindowsToWorkspace
  where
    (ringMask, littleMask, thumbMask) = fromFingersMask currentFingers

    keys =
      [ ((thumbMask .|. littleMask, xK_a), sinkAll)
      , ((thumbMask .|. littleMask, xK_c), kill)
      , ((thumbMask .|. littleMask, xK_d), menusMenu)
      , ((thumbMask .|. littleMask, xK_f), spawn "xfce4-find-cursor")
      , ((thumbMask .|. littleMask, xK_h), windows swapUp)
      , ((thumbMask .|. littleMask, xK_i), nextScreen)
      , ((thumbMask .|. littleMask, xK_l), windows swapDown)
      , ((thumbMask .|. littleMask, xK_m), xmodmapMenu)
      , ((thumbMask .|. littleMask, xK_n), sendMessage NextLayout)
      , ((thumbMask .|. littleMask, xK_r), recompileMenu)
      , ((thumbMask .|. ringMask, xK_c), spawn "light -U 3")
      , ((thumbMask .|. ringMask, xK_v), spawn "light -A 3")
      , ((thumbMask .|. ringMask, xK_f), spawn "amixer -c 1 set Master toggle && amixer -c 1 set Speaker unmute")
      , ((thumbMask .|. ringMask, xK_s), spawn "amixer -c 1 set Master 10-")
      , ((thumbMask .|. ringMask, xK_d), spawn "amixer -c 1 set Master 10+")
      , ((thumbMask .|. ringMask, xK_q), spawn "slock")
      , ((thumbMask .|. ringMask, xK_w), spawn "systemctl suspend ; slock")
      , ((thumbMask .|. ringMask, xK_e), spawn "systemctl hibernate ; slock")
      -- TODO: できない。やりたい。
      --, ((ringMask .|. littleMask, xK_h), spawn "xdotool key --window $(xdotool getactivewindow) Left")
      --, ((ringMask .|. littleMask, xK_j), spawn "xdotool key --window $(xdotool getactivewindow) Down")
      --, ((ringMask .|. littleMask, xK_k), spawn "xdotool key --window $(xdotool getactivewindow) Up")
      --, ((ringMask .|. littleMask, xK_l), spawn "xdotool key --window $(xdotool getactivewindow) Right")
      , ((thumbMask, xK_h), windows focusUp)
      , ((thumbMask, xK_j), withFocused $ sendMessage . MergeAll)
      , ((thumbMask, xK_k), withFocused $ sendMessage . UnMerge)
      , ((thumbMask, xK_l), windows focusDown)
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
      , ((ringMask, xK_v), withHomeDir $ spawn . (<> "/bin/gvim.sh"))
      -- Another KeyMask
      , ((shiftMask, xK_F1), xmodmapMenu)
      , ((shiftMask, xK_F2), withHomeDir $ spawn . (<> "/bin/dunst-swap-screen.sh"))
      , ((shiftMask, xK_F3), withHomeDir $ spawn . (<> "/bin/dzen2statusbar.sh"))
      , ((noModMask, xK_Print), takeScreenShot ActiveWindow)
      , ((shiftMask, xK_Print), takeScreenShot FullScreen)
      ]

    -- NOTE: 1 is omitted because it is hard to type
    switchingWorkspaces = do
        (numKey, workspace) <- zip [xK_2 .. xK_9] myWorkspaces
        let keymap = (thumbMask .|. littleMask, numKey)
        let switching = windows $ greedyView workspace
        pure (keymap, switching)

    puttingWindowsToWorkspace = do
      (numKey, workspace) <- zip [xK_2 .. xK_9] myWorkspaces
      let keymap = (ringMask, numKey)
      let putting = windows $ shift workspace
      pure (keymap, putting)
