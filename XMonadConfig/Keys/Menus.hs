{-# LANGUAGE QuasiQuotes #-}

module XMonadConfig.Keys.Menus where

import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString (..))
import Data.String.Here (i)
import Data.Text (Text)
import Shelly (Sh, shelly)
import qualified Shelly as Sh
import System.Directory (listDirectory)
import System.Environment (getEnv)
import XMonad
import XMonad.Prompt (ComplFunction, XPConfig (..), XPPosition (..), greenXPConfig)
import XMonad.Prompt.Input (inputPromptWithCompl, (?+))
import XMonadConfig.Keys.FingersMask (hhkbLiteFamilyFingers, writeFingerPref)

default (Text)

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
-- Dependency:
-- - xfce4-screenshooter
--
-- Optional:
-- - espeak
-- - notify-send
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
recompileMenu = flip catchError recover $
  inputPromptWithCompl myXPConf "Reload and restart xmonad?" yesNo ?+ \case
    "yes" -> shelly . withHomeDir' $ \homeDir -> do
      let replace = Sh.fromText (homeDir :: Text) <> "/.xmonad/replace.hs"
      Sh.run_ "notify-send" ["Recompiling..."]
      Sh.run_ replace []
      Sh.run_ "notify-send" ["end"]
    "no" -> pure ()
    x ->
      spawn
        [i|notify-send 'Please confirm with "yes" or "no": you took "${x}"'|]
  where
    withHomeDir' :: (Text -> Sh ()) -> Sh ()
    withHomeDir' = withHomeDir

    recover e = spawn [i|notify-send '${e}'|]

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
    "eDP-1" -> xrandrRescaleMenu "eDP-1"
    "HDMI-1" -> xrandrRescaleMenu "HDMI-1"
    "DP-1" -> xrandrRescaleMenu "DP-1"
    "DVI-I-1-1" -> xrandrRescaleMenu "DVI-I-1-1"
    "slock" -> spawn "slock"
    x -> spawn [i|notify-send 'unknown menu: ${x}'|]
  where
    menus _ = pure
      [ "HDMI-1"
      , "eDP-1"
      , "DP-1"
      , "DVI-I-1-1"
      , "start_dzen2"
      , "xmodmaps"
      , "finger_layouts"
      , "slock"
      ]

xrandrRescaleMenu :: String -> X ()
xrandrRescaleMenu output =
  inputPromptWithCompl myXPConf [i|xrandr --output ${output} --scale {your choice}|] scales ?+ \scale ->
    spawn [i|xrandr --output ${output} --scale ${scale} 2>&1 | xargs notify-send|]
    where
      scales _ = pure ["0.6x0.6", "0.8x0.8", "1.0x1.0", "1.2x1.2", "1.6x1.6"]
