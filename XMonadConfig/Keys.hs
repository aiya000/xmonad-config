{-# LANGUAGE OverloadedStrings #-}

-- |
-- For 'keys'.
--
-- Please see 'myNormalKeys' (the keymappings for my casual keyboards)
-- and 'myUnixKeys' (for HHKB like keyboards)
module XMonadConfig.Keys
  ( Keys
  , XKeyLayoutType
  , myNormalKeys
  , myUnixKeys
  , switchKeyModeTo
  , altMask
  , superMask
  , unixCasualMask
  , readMyKeys
  ) where

import Data.Map.Lazy (Map)
import Shelly (run_)
import System.EasyFile (doesFileExist)
import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.FloatKeys (keysMoveWindow)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Layout (ChangeLayout(..))
import XMonad.Layout.SubLayouts (GroupMsg(..))
import XMonad.Operations (sendMessage, withFocused)
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown, greedyView, shift)
import XMonadConfig.CommandWrapper
import XMonadConfig.Types (FilePath')
import XMonadConfig.XConfig (myTerminal, myWebBrowser, myWorkspaces)
import qualified Data.Map.Lazy as M

type Keys = XConfig Layout -> Map (KeyMask, KeySym) (X ())

data XKeyLayoutType = Common | UnixKeymap


altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask

unixCasualMask :: KeyMask
unixCasualMask = controlMask .|. shiftMask


-- |
-- Read the current condition of key layout type.
-- This condition is changed by 'switchKeyModeTo'.
readMyKeys :: IO (KeyMask, Keys)
readMyKeys = do
  inUnixKeymapMode <- currentKeyModeIs UnixKeymap
  return $ if inUnixKeymapMode
              then (unixCasualMask, myUnixKeys)
              else (superMask, myNormalKeys)
  where
    -- Return True if current XKeyLayoutType is specified argument.
    -- Otherwise, return False
    currentKeyModeIs :: XKeyLayoutType -> IO Bool
    currentKeyModeIs UnixKeymap = doesFileExist unixKeymapModeFlagFile
    currentKeyModeIs Common     = not <$> doesFileExist unixKeymapModeFlagFile

-- |
-- Restart xmonad-config
-- and load XMonadConfig.myNormalKeys or myUnixKeys
--
-- Warning: This is not working fine at now
switchKeyModeTo :: XKeyLayoutType -> X ()
switchKeyModeTo UnixKeymap = runShellyOnX $ body `continueIfFailed` notifyFailure
  where
    body = run_ "touch" [unixKeymapModeFlagFile]
      `continueIfSucceed` run_ "xmonad-config" ["--restart"]
      `continueIfSucceed` run_ "notify-send" ["XMonad", "restarted"]
    notifyFailure = run_ "notify-send" ["XMonad", "xmonad-config restarting is failed"]

switchKeyModeTo Common = runShellyOnX $ body `continueIfFailed` notifyFailure
  where
    body = run_ "rm" ["-f", unixKeymapModeFlagFile]
      `continueIfSucceed` run_ "xmonad-config" ["--restart"]
      `continueIfSucceed` run_ "notify-send" ["XMonad", "restarted"]
    notifyFailure = run_ "notify-send" ["XMonad", "xmonad-config restarting is failed"]

-- |
-- If this is exists, XMonadConfig.myUnixKeys will be loaded.
-- Otherwise, XMonadConfig.myNormalKeys will be loaded
unixKeymapModeFlagFile :: FilePath'
unixKeymapModeFlagFile = "/tmp/xmonad-keymode-UnixKeymap"


myNormalKeys :: Keys
myNormalKeys _ = M.fromList $
  [ ((altMask .|. controlMask, xK_a), sinkAll)
  , ((altMask .|. controlMask, xK_b), sendMessage ToggleStruts)
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
  , ((superMask, xK_F1), spawn "light -U 10")
  , ((superMask, xK_F2), spawn "light -A 10")
  , ((superMask, xK_F3), toggleTouchPad)
  , ((superMask, xK_F4), spawn "amixer -c 1 set Master toggle && amixer -c 1 set Speaker unmute")
  , ((superMask, xK_F5), spawn "amixer -c 1 set Master 3-")
  , ((superMask, xK_F6), spawn "amixer -c 1 set Master 3+")
  , ((superMask, xK_F10), lockScreen)
  , ((superMask, xK_F11), lockScreenSuspend)
  , ((superMask, xK_F12), lockScreenHibernate)
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
  , ((noModMask, xK_F1), resetXKeyboardLayout USKeyboardLayout)
  , ((shiftMask, xK_F1), resetXKeyboardLayout ResetSetXKBMAP)
  , ((noModMask, xK_Print), takeScreenShot FullScreen)
  , ((shiftMask, xK_Print), takeScreenShot ActiveWindow)
  , ((unixCasualMask, xK_x), switchKeyModeTo UnixKeymap)
  ]
  -- Switch workspace
  ++ [ ((altMask .|. controlMask, numKey), windows $ greedyView workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
     ]
  -- Move current window to target worskpace
  ++ [((superMask, numKey), windows $ shift workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
     ]


myUnixKeys :: Keys
myUnixKeys _ = M.fromList $
  [ ((unixCasualMask .|. altMask, xK_h), windows swapUp)
  , ((unixCasualMask .|. altMask, xK_l), windows swapDown)
  , ((unixCasualMask, xK_a), sinkAll)
  , ((unixCasualMask, xK_c), kill)
  , ((unixCasualMask, xK_e), spawn "thunar")
  , ((unixCasualMask, xK_f), spawn myWebBrowser)
  , ((unixCasualMask, xK_g), sendMessage NextLayout)
  , ((unixCasualMask, xK_h), windows focusUp)
  , ((unixCasualMask, xK_i), nextScreen)
  , ((unixCasualMask, xK_j), withFocused $ sendMessage . MergeAll)
  , ((unixCasualMask, xK_k), withFocused $ sendMessage . UnMerge)
  , ((unixCasualMask, xK_l), windows focusDown)
  , ((unixCasualMask, xK_m), spawn "pavucontrol")
  , ((unixCasualMask, xK_r), spawn "dmenu_run")
  , ((unixCasualMask, xK_t), spawn myTerminal)
  , ((unixCasualMask, xK_x), switchKeyModeTo Common)
  , ((superMask, xK_F1), spawn "light -U 10")
  , ((superMask, xK_F2), spawn "light -A 10")
  , ((superMask, xK_F3), spawn "amixer -c 1 set Master 3-")
  , ((superMask, xK_F4), spawn "amixer -c 1 set Master 3+")
  , ((superMask, xK_F10), lockScreen)
  , ((superMask, xK_F11), lockScreenSuspend)
  , ((superMask, xK_F12), lockScreenHibernate)
  -- Another KeyMask
  , ((noModMask, xK_Print), takeScreenShot FullScreen)
  , ((shiftMask, xK_Print), takeScreenShot ActiveWindow)
  ]
  -- Switch workspace
  ++ [ ((unixCasualMask, numKey), windows $ greedyView workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
     ]
  -- Move current window to target worskpace
  ++ [((altMask, numKey), windows $ shift workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
     ]
