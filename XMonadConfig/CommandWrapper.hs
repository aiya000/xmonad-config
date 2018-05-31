{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

-- | The cli command wrappers of X
module XMonadConfig.CommandWrapper
  ( takeScreenShot
  , ScreenShotType (..)
  , setKeyLayout
  , XKeyboardLayout (..)
  , withHomeDir
  , restartXMonadConfig
  , continueIfSucceed
  , continueIfFailed
  , runShellyOnX
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (fromString)
import Data.String.Here (i)
import Data.Typeable (cast)
import Shelly (Sh, shelly, run_, lastExitCode, exit, (</>))
import System.Environment (getEnv, lookupEnv)
import Text.Printf (printf)
import XMonad.Core (X, spawn)
import XMonadConfig.Types (FilePath')
import qualified Shelly as SH

-- | See `takeScreenShot`
data ScreenShotType = FullScreen | ActiveWindow

-- | See `setKeymapToUS`
data XKeyboardLayout = USKeyboardLayout
                     | JPKeyboardLayout
                     | ResetSetXKBMAP

asXkbmapStuff :: XKeyboardLayout -> String
asXkbmapStuff USKeyboardLayout = "us"
asXkbmapStuff JPKeyboardLayout = "jp"
asXkbmapStuff ResetSetXKBMAP   = "us" -- us by default

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
-- Reinstall xmonad-config.
-- If reinstalling is failed, notify it to notifyd
--
-- Dependency: notify-send
restartXMonadConfig :: X ()
restartXMonadConfig = runShellyOnX . withHomeDir' $ \homeDir' ->
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
