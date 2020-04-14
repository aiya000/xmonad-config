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
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Layout (ChangeLayout (..))
import XMonad.Layout.SubLayouts (GroupMsg (..))
import XMonad.Operations (sendMessage, withFocused)
import XMonad.StackSet (focusDown, focusUp, greedyView, shift, swapDown, swapUp)
import XMonadConfig.Keys.FingersMask (FingersMask, fromFingersMask)
import XMonadConfig.Keys.Menus
import XMonadConfig.XConfig (myTerminal, myWebBrowser, myWorkspaces)

type Keys = XConfig Layout -> Map (KeyMask, KeySym) (X ())

myToggleStrutsKey :: FingersMask -> XConfig l -> (KeyMask, KeySym)
myToggleStrutsKey fingers _ =
  let (_, littleMask, thumbMask) = fromFingersMask fingers
  in (thumbMask .|. littleMask, xK_g)

myKeys :: FingersMask -> Keys
myKeys fingers _ = M.fromList $
  keys <>
  switchingWorkspaces <>
  puttingWindowsToWorkspace
  where
    (ringMask, littleMask, thumbMask) = fromFingersMask fingers

    keys =
      [ ((thumbMask .|. littleMask, xK_a), sinkAll)
      , ((thumbMask .|. littleMask, xK_c), kill)
      , ((thumbMask .|. littleMask, xK_f), spawn "xfce4-find-cursor")
      , ((thumbMask .|. littleMask, xK_h), windows swapUp)
      , ((thumbMask .|. littleMask, xK_i), nextScreen)
      , ((thumbMask .|. littleMask, xK_l), windows swapDown)
      , ((thumbMask .|. littleMask, xK_m), xmodmapMenu)
      , ((thumbMask .|. littleMask, xK_n), sendMessage NextLayout)
      , ((thumbMask .|. littleMask, xK_r), withHomeDir $ spawn . (<> "/.xmonad/bin/xrandr-rotate.sh"))
      , ((thumbMask .|. ringMask, xK_a), withHomeDir $ spawn . (<> "/bin/autokey-gtk.sh"))
      , ((thumbMask .|. ringMask, xK_b), spawn myWebBrowser)
      , ((thumbMask .|. ringMask, xK_c), spawn "light -U 3")
      , ((thumbMask .|. ringMask, xK_d), spawn "pamixer --increase 10")
      , ((thumbMask .|. ringMask, xK_e), spawn "thunar")
      , ((thumbMask .|. ringMask, xK_f), spawn "[ $(pamixer --get-mute) = 'true' ] && pamixer --unmute || pamixer --mute")
      , ((thumbMask .|. ringMask, xK_g), withHomeDir $ spawn . (<> "/bin/gvim.sh"))
      , ((thumbMask .|. ringMask, xK_m), spawn "pavucontrol")
      , ((thumbMask .|. ringMask, xK_q), spawn "slock")
      , ((thumbMask .|. ringMask, xK_s), spawn "pamixer --decrease 10")
      , ((thumbMask .|. ringMask, xK_t), spawn myTerminal)
      , ((thumbMask .|. ringMask, xK_v), spawn "light -A 3")
      , ((thumbMask .|. ringMask, xK_w), spawn "systemctl suspend ; slock")
      , ((thumbMask .|. ringMask, xK_x), withHomeDir $ spawn . (<> "/.xmonad/bin/trackpad-toggle.sh"))
      -- TODO: できない。やりたい。
      --, ((ringMask .|. littleMask, xK_h), spawn "xdotool key --window $(xdotool getactivewindow) Left")
      --, ((ringMask .|. littleMask, xK_j), spawn "xdotool key --window $(xdotool getactivewindow) Down")
      --, ((ringMask .|. littleMask, xK_k), spawn "xdotool key --window $(xdotool getactivewindow) Up")
      --, ((ringMask .|. littleMask, xK_l), spawn "xdotool key --window $(xdotool getactivewindow) Right")
      , ((thumbMask, xK_h), windows focusUp)
      , ((thumbMask, xK_j), withFocused $ sendMessage . MergeAll)
      , ((thumbMask, xK_k), withFocused $ sendMessage . UnMerge)
      , ((thumbMask, xK_l), windows focusDown)
      -- Another KeyMask
      , ((ringMask, xK_r), spawn "dmenu_run")
      , ((shiftMask, xK_F1), xmodmapMenu)
      , ((shiftMask, xK_F2), withHomeDir $ spawn . (<> "/bin/dunst-swap-screen.sh"))
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
