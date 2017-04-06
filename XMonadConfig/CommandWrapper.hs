-- | CLI command wrappers for X
module XMonadConfig.CommandWrapper
  ( takeScreenShot
  , ScreenShotType (..)
  , lockScreen
  , lockScreenSuspend
  , lockScreenHibernate
  , toggleTouchPad
  , setXKeyboardLayout
  , XKeyboardLayout (..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Text.Printf (printf)
import XMonad.Core (X, spawn)

-- | See `takeScreenShot`
data ScreenShotType = FullScreen | ActiveWindow

-- | See `setKeymapToUS`
data XKeyboardLayout = USKeyboardLayout


-- |
-- Take screenshot as ScreenShotType to ~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png,
-- and notify to finish as screen and voice message
--
-- Dependency: imagemagick, espeak, notify-send, xdotool
takeScreenShot :: ScreenShotType -> X ()
takeScreenShot ssType = do
  let msg = messageOf ssType
  screenshot ssType dateSSPath
  spawn  $ printf "espeak -s 150 -v +fex '%s'" msg
  liftIO $ threadDelay 1000000  -- Wait 1 sec
  spawn  $ printf "notify-send 'ScreenShot' '%s'" msg
  where
    screenshot :: ScreenShotType -> FilePath -> X ()
    screenshot FullScreen   path = spawn $ printf "import -window root %s" path
    screenshot ActiveWindow path = spawn $ printf "import -window $(xdotool getwindowfocus -f) %s" path

    dateSSPath             = "~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png"
    messageOf FullScreen   = "shot the full screen"
    messageOf ActiveWindow = "shot the active window"


-- |
-- Execute xflock4
--
-- Dependency: xflock4
lockScreen :: X ()
lockScreen = spawn "xflock4"

-- |
-- Execute xscreensaver-command -lock and sudo pm-suspend
--
-- Dependency: xflock4, xfce4-session-logout
lockScreenSuspend :: X ()
lockScreenSuspend = do
  lockScreen
  spawn "xfce4-session-logout --suspend"

-- |
-- Execute xscreensaver-command -lock and sudo pm-hibernate
--
-- Notice: pm-hibernate must be added to sudoers with you and NOPASSWD
--
-- Dependency: xflock4, sudo, pm-hibernate
lockScreenHibernate :: X ()
lockScreenHibernate = do
  lockScreen
  spawn "sleep 2"
  --spawn "xfce4-session-logout --hibernate"  --NOTE: Doesn't work
  spawn "sudo pm-hibernate"


-- |
-- Disable touch pad if touch pad is enabled.
-- Enable touch pad if touch pad is disabled
--
-- Notice: This is not working fine if you link this repository to other than ~/.xmonad
toggleTouchPad :: X ()
toggleTouchPad = do
  homeDir <- liftIO $ getEnv "HOME"
  spawn $ homeDir ++ "/.xmonad/bin/trackpad-toggle.sh"


-- |
-- Change keyboard layout to 'us' and swap ctrl and caps
--
-- Dependency: setxkbmap, notify-send
setXKeyboardLayout :: XKeyboardLayout -> X ()
setXKeyboardLayout USKeyboardLayout = do
  spawn "setxkbmap -layout us -option ctrl:swapcaps"
  spawn "notify-send 'Keyboard Layout' 'Current KEYMAP is us'"
