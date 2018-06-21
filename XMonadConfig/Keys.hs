{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

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

import Control.Concurrent (threadDelay)
import Data.Map.Lazy (Map)
import Data.Semigroup ((<>))
import Data.String (IsString(..))
import Data.String.Here (i)
import System.Environment (getEnv, lookupEnv)
import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.FloatKeys (keysMoveWindow)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Layout (ChangeLayout(..))
import XMonad.Layout.SubLayouts (GroupMsg(..))
import XMonad.Operations (sendMessage, withFocused)
import XMonad.Prompt (XPConfig(..), XPPosition(..), greenXPConfig)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown, greedyView, shift)
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

myXPConf :: XPConfig
myXPConf = greenXPConfig
            { font = "xft:Ricty:Regular:size=10:antialias=true"
            , position = Top
            }


myKeys :: Keys
myKeys _ = M.fromList $
  [ ((thumbMask .|. littleMask, xK_a), sinkAll)
  , ((thumbMask .|. littleMask, xK_c), kill)
  , ((thumbMask .|. littleMask, xK_h), windows swapUp)
  , ((thumbMask .|. littleMask, xK_i), nextScreen)
  , ((thumbMask .|. littleMask, xK_l), windows swapDown)
  , ((thumbMask .|. littleMask, xK_n), sendMessage NextLayout)
  , ((thumbMask .|. littleMask, xK_r), replaceXMonadWithConfirm)
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
  --, ((shiftMask, xK_F3), nextXModMap)
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
  where
    replaceXMonadWithConfirm :: X ()
    replaceXMonadWithConfirm =
      confirmPrompt myXPConf "Reload and restart xmonad?" $
        withHomeDir $ spawn . (<> "/.xmonad/replace.sh")


-- | Polymorphic string
type FilePath' = forall s. IsString s => s

-- |
-- Shortcut for @$HOME@ .
-- Apply @$HOME@ to @f@
withHomeDir :: MonadIO m => (FilePath' -> m a) -> m a
withHomeDir f = do
  homeDir <- liftIO $ getEnv "HOME"
  f $ fromString homeDir

-- | See `setKeymapToUS`
data XKeyboardLayout = USKeyboardLayout
                     | JPKeyboardLayout
                     | ResetSetXKBMAP

asXkbmapStuff :: XKeyboardLayout -> String
asXkbmapStuff USKeyboardLayout = "us"
asXkbmapStuff JPKeyboardLayout = "jp"
asXkbmapStuff ResetSetXKBMAP   = "us" -- us by default


-- |
-- 
-- Change keyboard layout.
--
-- Read a value of $XMONAD_CONFIG_SETXKBMAP_OPTIONS,
-- and apply it.
--
-- The example value of $XMONAD_CONFIG_SETXKBMAP_OPTIONS is '-option caps:ctrl_modifier'
--
-- Dependency: setxkbmap, notify-send
setKeyLayout :: XKeyboardLayout -> X ()
setKeyLayout ResetSetXKBMAP = do
  spawn "notify-send 'Keyboard Layout' 'KEYMAP is reset'"
  spawn "setxkbmap -option"
setKeyLayout (asXkbmapStuff -> layout) = do
  maybeOpt <- liftIO $ lookupEnv "XMONAD_CONFIG_SETXKBMAP_OPTIONS"
  case maybeOpt of
    Nothing  -> spawn "notify-send 'Failed' 'XMONAD_CONFIG_SETXKBMAP_OPTIONS is never set'"
    Just opt -> do
      spawn [i|setxkbmap -layout ${layout} ${opt}|]
      spawn "xmodmap ~/.Xmodmap"
      spawn [i|notify-send 'Keyboard Layout' 'The current KEYMAP is ${layout}'|]

-- | See `takeScreenShot`
data ScreenShotType = FullScreen | ActiveWindow

-- |
-- Take screenshot as ScreenShotType to ~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png,
-- and notify to finish as screen and voice message
--
-- Dependency: imagemagick, espeak, notify-send, xdotool
takeScreenShot :: ScreenShotType -> X ()
takeScreenShot ssType = do
  let msg = messageOf ssType
  screenshot ssType dateSSPath
  spawn [i|espeak -s 150 -v +fex '${msg}'|]
  liftIO $ threadDelay aSec
  spawn [i|notify-send 'ScreenShot' '${msg}'|]
  where
    aSec = 1000000
    dateSSPath = "~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png"

    screenshot :: ScreenShotType -> FilePath -> X ()
    screenshot FullScreen   path = spawn [i|import -window root ${path}|]
    screenshot ActiveWindow path = spawn [i|import -window $(xdotool getwindowfocus -f) ${path}|]

    messageOf :: ScreenShotType -> String
    messageOf FullScreen   = "shot the full screen"
    messageOf ActiveWindow = "shot the active window"
