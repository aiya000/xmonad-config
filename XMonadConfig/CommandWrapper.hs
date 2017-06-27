{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | The cli command wrappers of X
module XMonadConfig.CommandWrapper
  ( takeScreenShot
  , ScreenShotType (..)
  , lockScreen
  , lockScreenSuspend
  , lockScreenHibernate
  , toggleTouchPad
  , resetXKeyboardLayout
  , XKeyboardLayout (..)
  , XMonadConfigKeyMode (..)
  , switchKeyModeTo
  , currentKeyModeIs
  , restartXMonadConfig
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (IsString, fromString)
import Data.Typeable (cast)
import Shelly (Sh, shelly, run_, lastExitCode, exit, (</>))
import System.EasyFile (doesFileExist)
import System.Environment (getEnv, lookupEnv)
import Text.Printf (printf)
import XMonad.Core (X, spawn)
import qualified Shelly as SH

-- | See `takeScreenShot`
data ScreenShotType = FullScreen | ActiveWindow

-- | See `setKeymapToUS`
data XKeyboardLayout = USKeyboardLayout

-- | Polymorphic string
type FilePath' = forall s. IsString s => s

-- | See `switchKeyModeTo`
data XMonadConfigKeyMode = Common | UnixKeymap


-- |
-- Shortcut for @$HOME@ .
-- Apply @$HOME@ to @f@
withHomeDir :: MonadIO m => (FilePath' -> m a) -> m a
withHomeDir f = do
  homeDir <- liftIO $ getEnv "HOME"
  f $ fromString homeDir


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
    dateSSPath = "~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png"

    screenshot :: ScreenShotType -> FilePath -> X ()
    screenshot FullScreen   path = spawn $ printf "import -window root %s" path
    screenshot ActiveWindow path = spawn $ printf "import -window $(xdotool getwindowfocus -f) %s" path

    messageOf :: ScreenShotType -> String
    messageOf FullScreen   = "shot the full screen"
    messageOf ActiveWindow = "shot the active window"


-- |
-- Execute xflock4
--
-- Dependency: xflock4
lockScreen :: X ()
lockScreen = spawn "xflock4"

-- |
-- Execute xflock4 and xfce4-session-logout --suspend
--
-- Dependency: xflock4, xfce4-session-logout
lockScreenSuspend :: X ()
lockScreenSuspend = do
  lockScreen
  spawn "xfce4-session-logout --suspend"

-- |
-- Execute xflock4 and sudo pm-hibernate
--
-- Notice: pm-hibernate must be added to sudoers with you and NOPASSWD
--
-- Dependency: xflock4, sudo, pm-hibernate
lockScreenHibernate :: X ()
lockScreenHibernate = do
  lockScreen
  spawn "sleep 2"
  --spawn "xfce4-session-logout --hibernate" -- NOTE: Doesn't work
  spawn "sudo pm-hibernate"


-- |
-- Disable touch pad if touch pad is enabled.
-- Enable touch pad if touch pad is disabled
--
-- Notice: This is not working fine if you link this repository to other than ~/.xmonad
toggleTouchPad :: X ()
toggleTouchPad = withHomeDir $ \homeDir -> do
  spawn $ homeDir ++ "/.xmonad/bin/trackpad-toggle.sh"


-- |
-- 
-- Change keyboard layout to 'us'.
-- And read a value of $XMONAD_CONFIG_SETXKBMAP_OPTIONS,
-- apply it.
--
-- The example value of $XMONAD_CONFIG_SETXKBMAP_OPTIONS is '-option caps:ctrl_modifier'
--
-- Dependency: setxkbmap, notify-send
resetXKeyboardLayout :: XKeyboardLayout -> X ()
resetXKeyboardLayout USKeyboardLayout = do
  maybeOpt <- liftIO $ lookupEnv "XMONAD_CONFIG_SETXKBMAP_OPTIONS"
  case maybeOpt of
    Nothing  -> spawn "notify-send 'Failed' 'XMONAD_CONFIG_SETXKBMAP_OPTIONS is not set'"
    Just opt -> do
      spawn $ "setxkbmap -layout us " ++ opt
      spawn $ "xmodmap ~/.Xmodmap"
      spawn "notify-send 'Keyboard Layout' 'Current KEYMAP is us'"


-- | Instead of '&&' in shelly
continueIfSucceed :: Sh a -> Sh b -> Sh a
x `continueIfSucceed` y = do
  result   <- x
  exitCode <- lastExitCode
  when (exitCode == 0) $ void y
  return result
infixl 3 `continueIfSucceed`

-- | Instead of '||' in shelly
continueIfFailed :: Sh a -> Sh b -> Sh a
x `continueIfFailed` y = do
  result   <- x
  exitCode <- lastExitCode
  when (exitCode /= 0) $ void y
  return result
infixl 3 `continueIfFailed`


-- | Execute `Sh a` in X monad context
runShellyOnX :: Sh a -> X ()
runShellyOnX = liftIO . shelly . void


-- |
-- If this is exists, XMonadConfig.myUnixKeys will be loaded.
-- Otherwise, XMonadConfig.myNormalKeys will be loaded
unixKeymapModeFlagFile :: FilePath'
unixKeymapModeFlagFile = "/tmp/xmonad-keymode-UnixKeymap"

-- |
-- Restart xmonad-config
-- and load XMonadConfig.myNormalKeys or myUnixKeys
--
-- Warning: This is not working fine at now
switchKeyModeTo :: XMonadConfigKeyMode -> X ()
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
-- Return True if current XMonadConfigKeyMode is specified argument.
-- Otherwise, return False
currentKeyModeIs :: XMonadConfigKeyMode -> IO Bool
currentKeyModeIs UnixKeymap = doesFileExist unixKeymapModeFlagFile
currentKeyModeIs Common     = not <$> doesFileExist unixKeymapModeFlagFile


-- |
-- Reinstall xmonad-config.
-- If reinstalling is failed, notify it to notifyd
--
-- Dependency: notify-send
restartXMonadConfig :: X ()
restartXMonadConfig = runShellyOnX . withHomeDir' $ \homeDir' -> do
  body (cast' homeDir') `continueIfFailed` notifyFailure
  where
    withHomeDir' :: (FilePath -> Sh ()) -> Sh ()
    withHomeDir' = withHomeDir

    cast' :: FilePath -> Maybe SH.FilePath
    cast' = cast

    body :: Maybe SH.FilePath -> Sh ()
    body Nothing = exit 1
    body (Just homeDir) = do
      let buildScript = homeDir </> "/build"
      run_ "notify-send" ["XMonad", "xmonad restarteding is started"]
        `continueIfSucceed` run_ buildScript []
        `continueIfSucceed` run_ "notify-send" ["XMonad", "xmonad restarting is done"]

    notifyFailure = run_ "notify-send" ["XMonad", "xmonad restarting is failed"]
