{-# LANGUAGE QuasiQuotes #-}

module XMonadConfig.Keys.Menus where

import           Control.Monad                 (void)
import           Control.Monad.IO.Class        (MonadIO)
import           Data.String                   (IsString (..))
import           Data.String.Here              (i)
import           System.Directory              (listDirectory)
import           System.Environment            (getEnv)
import           XMonad
import           XMonad.Prompt                 (ComplFunction, XPConfig (..),
                                                XPPosition (..), greenXPConfig,
                                                mkComplFunFromList')
import           XMonad.Prompt.Input           (inputPromptWithCompl, (?+))
import           XMonadConfig.Keys.FingersMask (hhkbLiteFamilyFingers,
                                                writeFingerPref)

-- | Polymorphic string
type FilePath'
   = forall s. IsString s =>
                 s

-- |
-- Shortcut for @$HOME@ .
-- Apply @$HOME@ to @f@
withHomeDir :: MonadIO m => (FilePath' -> m a) -> m a
withHomeDir f = do
  homeDir <- liftIO $ getEnv "HOME"
  f $ fromString homeDir

-- | See `takeScreenShot`
data ScreenShotType
  = FullScreen
  | ActiveWindow
  deriving (Show)

-- |
-- Take screenshot as ScreenShotType to ~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png,
-- and notify to finish as screen and voice message
--
-- Dependency: imagemagick, espeak, notify-send, xdotool
takeScreenShot :: ScreenShotType -> X ()
takeScreenShot x =
  withHomeDir $ \homeDir ->
    spawn [i|${homeDir :: String}/.xmonad/bin/screenshot.sh ${x}|]

myXPConf :: XPConfig
myXPConf =
  greenXPConfig
    {font = "xft:Ricty:Regular:size=10:antialias=true", position = Top}

fingerLayoutMenu :: X ()
fingerLayoutMenu =
  void $
  inputPromptWithCompl myXPConf "Change keymasks" fingerLayouts ?+ \case
    "HHKB_Lite2_Family" -> writeFingerPref hhkbLiteFamilyFingers
    "default_(Surface_type_cover)" -> writeFingerPref def
    x -> spawn [i|notify-send '"${x}" is an unknown finger layout'|]
  where
    fingerLayouts :: ComplFunction
    fingerLayouts =
      mkComplFunFromList' ["HHKB_Lite2_Family", "default_(Surface_type_cover)"]

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
yesNo = mkComplFunFromList' ["yes", "no"]

-- | Load a .xmodmap
xmodmapMenu :: X ()
xmodmapMenu =
  void $
  inputPromptWithCompl myXPConf "Load a .xmodmap" xmonadXmodmaps ?+ \xmodmapFile ->
    spawn
      [i| -- setxkbmap must be done before xmodmap, because setxkbmap overwrites.
      setxkbmap -layout us -option caps:ctrl_modifier &&
      xmodmap ~/.xmonad/Xmodmap/${xmodmapFile} &&
      notify-send '${xmodmapFile} did seem to be loaded :D'
    |]
    -- ~/.xmonad/Xmodmap/*
  where
    xmonadXmodmaps :: ComplFunction
    xmonadXmodmaps _ =
      withHomeDir $
      (filter (/= "README.md") <$>) . listDirectory . (<> "/.xmonad/Xmodmap")

-- | Meta/General menu
menusMenu :: X ()
menusMenu =
  inputPromptWithCompl myXPConf "General" menus ?+ \case
    "finger_layouts" -> fingerLayoutMenu
    "start_dzen2" -> withHomeDir $ spawn . (<> "/bin/dzen2statusbar.sh")
    "xmodmaps" -> xmodmapMenu
    x -> spawn [i|notify-send 'unknown menu: ${x}'|]
  where
    menus :: ComplFunction
    menus _ = pure ["start_dzen2", "xmodmaps", "finger_layouts"]
