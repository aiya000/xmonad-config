{-# LANGUAGE QuasiQuotes #-}

module XMonadConfig.Keys.Menus where

import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString (..))
import Data.String.Here (i)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import XMonad
import XMonad.Prompt (ComplFunction, XPConfig (..), XPPosition (..), greenXPConfig)
import XMonad.Prompt.Input (inputPromptWithCompl, (?+))
import XMonadConfig.Keys.FingersMask (hhkbLiteFamilyFingers, writeFingerPref)

-- | Polymorphic string
type FilePath' = forall s. IsString s => s

-- |
-- Shortcut for @$HOME@ .
-- Apply @$HOME@ to @f@
withHomeDir :: MonadIO m => (FilePath' -> m a) -> m a
withHomeDir f = do
  homeDir <- liftIO $ getEnv "HOME"
  f $ fromString homeDir

-- | See `takeScreenShot`
data ScreenShotType = FullScreen
                    | ActiveWindow
  deriving (Show)

-- |
-- Take screenshot as ScreenShotType to ~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png,
-- and notify to finish as screen and voice message
--
-- Dependency: imagemagick, espeak, notify-send, xdotool
takeScreenShot :: ScreenShotType -> X ()
takeScreenShot x = withHomeDir $ \homeDir ->
  spawn [i|${homeDir :: String}/.xmonad/bin/screenshot.sh ${x}|]

myXPConf :: XPConfig
myXPConf = greenXPConfig
  { font = "xft:Ricty:Regular:size=10:antialias=true"
  , position = Top
  }

fingerLayoutMenu :: X ()
fingerLayoutMenu =
  inputPromptWithCompl myXPConf "Change keymasks" fingerLayouts ?+ \case
    "HHKB_Lite2_Family" -> writeFingerPref hhkbLiteFamilyFingers
    "default_(Surface_type_cover)" -> writeFingerPref def
    x -> spawn [i|notify-send '"${x}" is an unknown finger layout'|]
  where
    fingerLayouts _ = pure ["HHKB_Lite2_Family", "default_(Surface_type_cover)"]

-- | Compile, replace and restart this xmonad
recompileMenu :: X ()
recompileMenu =
  inputPromptWithCompl myXPConf "Reload and restart xmonad?" yesNo ?+ \case
    "yes" -> do
      withHomeDir $ spawn . (<> "/.xmonad/replace.sh")
      spawn [i|notify-send 'Recompiling...'|]
    "no" -> pure ()
    x ->
      spawn
        [i|notify-send 'Please confirm with "yes" or "no": you took "${x}"'|]

yesNo :: ComplFunction
yesNo _ = pure ["yes", "no"]

-- | Load a .xmodmap
xmodmapMenu :: X ()
xmodmapMenu =
  inputPromptWithCompl myXPConf "Load a .xmodmap" xmonadXmodmaps ?+ \xmodmapFile ->
    spawn
      [i| -- setxkbmap must be done before xmodmap, because setxkbmap overwrites.
      setxkbmap -layout us -option caps:ctrl_modifier &&
      xmodmap ~/.xmonad/Xmodmap/${xmodmapFile} &&
      notify-send '${xmodmapFile} did seem to be loaded :D'
    |]
    -- ~/.xmonad/Xmodmap/*
  where
    xmonadXmodmaps _ = withHomeDir $
      (filter (/= "README.md") <$>) . listDirectory . (<> "/.xmonad/Xmodmap")

-- | Meta/General menu
menusMenu :: X ()
menusMenu =
  inputPromptWithCompl myXPConf "General" menus ?+ \case
    "finger_layouts" -> fingerLayoutMenu
    "start_dzen2" -> withHomeDir $ spawn . (<> "/bin/dzen2statusbar.sh")
    "xmodmaps" -> xmodmapMenu
    "eDP-1" -> edp1Menu
    "HDMI-1" -> hdmi1Menu
    x -> spawn [i|notify-send 'unknown menu: ${x}'|]
  where
    menus _ = pure
      [ "HDMI-1"
      , "eDP-1"
      , "start_dzen2"
      , "xmodmaps"
      , "finger_layouts"
      ]

edp1Menu :: X ()
edp1Menu =
  inputPromptWithCompl myXPConf "xrandr --output eDP-1" scales ?+ \case
    "0.8x0.8" -> spawn [i|xrandr --output eDP-1 --scale 0.8x0.8|]
    "1.0x1.0" -> spawn [i|xrandr --output eDP-1 --scale 1.9x1.0|]
    x -> spawn [i|notify-send 'unknown scale: ${x}'|]
    where
      scales _ = pure ["0.8x0.8", "1.0x1.0"]

hdmi1Menu :: X ()
hdmi1Menu = inputPromptWithCompl myXPConf "xrandr --output HDMI-1" scales ?+ \case
  "1.2x1.2" -> spawn [i|xrandr --output HDMI-1 --scale 1.2x1.2|]
  "1.6x1.6" -> spawn [i|xrandr --output HDMI-1 --scale 1.6x1.6|]
  x -> spawn [i|notify-send 'unknown scale: ${x}'|]
  where
    scales _ = pure ["1.2x1.2", "1.6x1.6"]
