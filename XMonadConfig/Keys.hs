{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Expose 'myKeys' for 'XConfig.keys'.
--
-- This module exports compile time processings
-- for many keyboard layouts (e.g. HHKB Lite2 US, Surface type cover, HHKB Lite2 JP + HHKB Lite2 US)
module XMonadConfig.Keys where

import           Control.Monad            (void)
import qualified Data.ByteString.Char8    as ByteString
import           Data.Default             (Default (..))
import           Data.FileEmbed           (embedOneFileOf)
import           Data.Map.Lazy            (Map)
import qualified Data.Map.Lazy            as M
import           Data.Maybe               (fromMaybe)
import           Data.Semigroup           ((<>))
import           Data.String              (IsString (..))
import           Data.String.Here         (i)
import           Safe                     (readMay)
import           System.Directory         (listDirectory)
import           System.Environment       (getEnv)
import           XMonad
import           XMonad.Actions.CycleWS   (nextScreen)
import           XMonad.Actions.FloatKeys (keysMoveWindow)
import           XMonad.Actions.SinkAll   (sinkAll)
import           XMonad.Layout            (ChangeLayout (..))
import           XMonad.Layout.SubLayouts (GroupMsg (..))
import           XMonad.Operations        (sendMessage, withFocused)
import           XMonad.Prompt            (ComplFunction, XPConfig (..),
                                           XPPosition (..), greenXPConfig,
                                           mkComplFunFromList')
import           XMonad.Prompt.Input      (inputPromptWithCompl, (?+))
import           XMonad.StackSet          (focusDown, focusUp, greedyView,
                                           shift, swapDown, swapUp)
import           XMonadConfig.TH
import           XMonadConfig.XConfig     (myTerminal, myWebBrowser,
                                           myWorkspaces)

type Keys = XConfig Layout -> Map (KeyMask, KeySym) (X ())

altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask

-- | Serializable `KeyMask` for known key masks
data KeyMask'
  = AltMask
  | SuperMask
  | ShiftMask
  | ControlMask
  deriving (Show, Read)

fromKeyMask' :: KeyMask' -> KeyMask
fromKeyMask' AltMask     = altMask
fromKeyMask' SuperMask   = superMask
fromKeyMask' ShiftMask   = shiftMask
fromKeyMask' ControlMask = controlMask

-- | A serializable set of `KeyMask'`
data FingersMask = FingersMask
  { ringMask'   :: KeyMask' -- ^ the ring (finger) mask
  , littleMask' :: KeyMask' -- ^ the little (finger) mask
  , thumbMask'  :: KeyMask' -- ^ the thumb mask
  } deriving (Show, Read)

fromFingersMask :: FingersMask -> (KeyMask, KeyMask, KeyMask)
fromFingersMask FingersMask {..} =
  (fromKeyMask' ringMask', fromKeyMask' littleMask', fromKeyMask' thumbMask')

instance Default FingersMask
  -- | A finger masks layout for surface type cover
                                                    where
  def =
    FingersMask
      {ringMask' = SuperMask, littleMask' = ControlMask, thumbMask' = AltMask}

-- | A 'FingersMask' layout for HHKB Lite2 us keyboard
hhkbLiteFamilyFingers :: FingersMask
hhkbLiteFamilyFingers =
  FingersMask
    {ringMask' = AltMask, littleMask' = ControlMask, thumbMask' = SuperMask}

-- |
-- On the compile time,
-- load currentFingers file if it is existent,
-- or Load currentFingers-default (usually, currentFingers-default is existent),
-- or use `Default` of `FingersMask`.
currentFingers :: FingersMask
currentFingers =
  fromMaybe def . readMay $
  ByteString.unpack $(embedOneFileOf currentFingersPaths)

myToggleStrutsKey :: LayoutClass l Window => XConfig l -> (KeyMask, KeySym)
myToggleStrutsKey _ =
  let (_, littleMask, thumbMask) = fromFingersMask currentFingers
   in (thumbMask .|. littleMask, xK_g)

myXPConf :: XPConfig
myXPConf =
  greenXPConfig
    {font = "xft:Ricty:Regular:size=10:antialias=true", position = Top}

-- TODO: the promt for setxkbmap -option $XMONAD_CONFIG_SETXKBMAP_OPTIONS
myKeys :: Keys
myKeys _ =
  let (ringMask, littleMask, thumbMask) = fromFingersMask currentFingers
   in M.fromList $
      [ ((thumbMask .|. littleMask, xK_a), sinkAll)
      , ((thumbMask .|. littleMask, xK_c), kill)
      , ( (thumbMask .|. littleMask, xK_d)
        , withHomeDir $ spawn . (<> "/bin/dzen2statusbar.sh"))
      , ((thumbMask .|. littleMask, xK_f), spawn "xfce4-find-cursor")
      , ((thumbMask .|. littleMask, xK_h), windows swapUp)
      , ((thumbMask .|. littleMask, xK_i), nextScreen)
      , ((thumbMask .|. littleMask, xK_k), fingerLayoutMenu)
      , ((thumbMask .|. littleMask, xK_l), windows swapDown)
      , ((thumbMask .|. littleMask, xK_m), xmodmapMenu)
      , ((thumbMask .|. littleMask, xK_n), sendMessage NextLayout)
      , ((thumbMask .|. littleMask, xK_r), recompileMenu)
      , ((ringMask .|. littleMask, xK_s), spawn "amixer -c 1 set Master 10-") -- thumb+ring keys adjusts for MISTEL Barroco MD-600
      , ((ringMask .|. littleMask, xK_d), spawn "amixer -c 1 set Master 10+")
      , ( (ringMask .|. littleMask, xK_f)
        , spawn
            "amixer -c 1 set Master toggle && amixer -c 1 set Speaker unmute")
      , ((ringMask .|. littleMask, xK_c), spawn "light -U 5")
      , ((ringMask .|. littleMask, xK_v), spawn "light -A 5")
    --, ((ringMask .|. littleMask, xK_h), spawn "xdotool key --window $(xdotool getactivewindow) Left")
    --, ((ringMask .|. littleMask, xK_j), spawn "xdotool key --window $(xdotool getactivewindow) Down")
    --, ((ringMask .|. littleMask, xK_k), spawn "xdotool key --window $(xdotool getactivewindow) Up")
    --, ((ringMask .|. littleMask, xK_l), spawn "xdotool key --window $(xdotool getactivewindow) Right")
      , ((thumbMask, xK_h), windows focusUp)
      , ((thumbMask, xK_j), withFocused $ sendMessage . MergeAll)
      , ((thumbMask, xK_k), withFocused $ sendMessage . UnMerge)
      , ((thumbMask, xK_l), windows focusDown)
      , ((ringMask, xK_F1), spawn "light -U 5") -- ring+Fn keys adjusts for Surface type cover's seal
      , ((ringMask, xK_F2), spawn "light -A 5")
      , ( (ringMask, xK_F4)
        , spawn
            "amixer -c 1 set Master toggle && amixer -c 1 set Speaker unmute")
      , ((ringMask, xK_F5), spawn "amixer -c 1 set Master 10-")
      , ((ringMask, xK_F6), spawn "amixer -c 1 set Master 10+")
      , ((ringMask, xK_F10), spawn "systemctl hibernate ; slock")
      , ((ringMask, xK_F11), spawn "systemctl suspend ; slock")
      , ((ringMask, xK_F12), spawn "slock")
      , ( (ringMask, xK_c)
        , withHomeDir $ spawn . (<> "/.xmonad/bin/trackpad-toggle.sh"))
      , ((ringMask, xK_e), spawn "thunar")
      , ((ringMask, xK_f), spawn myWebBrowser)
      , ((ringMask, xK_h), withFocused $ keysMoveWindow (-5, 0))
      , ((ringMask, xK_j), withFocused $ keysMoveWindow (0, 5))
      , ((ringMask, xK_k), withFocused $ keysMoveWindow (0, -5))
      , ((ringMask, xK_l), withFocused $ keysMoveWindow (5, 0))
      , ((ringMask, xK_m), spawn "pavucontrol")
      , ((ringMask, xK_r), spawn "dmenu_run")
      , ((ringMask, xK_t), spawn myTerminal)
    -- Another KeyMask
      , ((shiftMask, xK_F1), xmodmapMenu)
      , ( (shiftMask, xK_F2)
        , withHomeDir $ spawn . (<> "/bin/dunst-swap-screen.sh"))
      , ( (shiftMask, xK_F3)
        , withHomeDir $ spawn . (<> "/bin/dzen2statusbar.sh"))
      , ((noModMask, xK_Print), takeScreenShot ActiveWindow)
      , ((shiftMask, xK_Print), takeScreenShot FullScreen)
      ]
    -- Switch a workspace
       ++
      [ ((thumbMask .|. littleMask, numKey), windows $ greedyView workspace)
      | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
      ]
    -- Move a current window to a worskpace
       ++
      [ ((ringMask, numKey), windows $ shift workspace)
      | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces
      ]
    -- Load a .xmodmap
  where
    xmodmapMenu :: X ()
    xmodmapMenu =
      void $
      inputPromptWithCompl myXPConf "Load a .xmodmap" xmonadXmodmaps ?+ \xmodmapFile ->
        spawn
          [i|
          setxkbmap -layout us -option caps:ctrl_modifier &&
          xmodmap ~/.xmonad/Xmodmap/${xmodmapFile} &&
          notify-send '${xmodmapFile} did seem to be loaded :D'
        |] -- setxkbmap must be done before xmodmap, because setxkbmap overwrites.
    -- ~/.xmonad/Xmodmap/*
    xmonadXmodmaps :: ComplFunction
    xmonadXmodmaps _ =
      withHomeDir $
      (filter (/= "README.md") <$>) . listDirectory . (<> "/.xmonad/Xmodmap")
    -- Compile, replace and restart this xmonad
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
    fingerLayoutMenu :: X ()
    fingerLayoutMenu =
      void $
      inputPromptWithCompl myXPConf "Change keymasks" fingerLayouts ?+ \case
        "HHKB_Lite2_Family" -> writeFingerPref hhkbLiteFamilyFingers
        "default_(Surface_type_cover)" -> writeFingerPref def
        x -> spawn [i|notify-send '"${x}" is an unknown finger layout'|]
    fingerLayouts :: ComplFunction
    fingerLayouts =
      mkComplFunFromList' ["HHKB_Lite2_Family", "default_(Surface_type_cover)"]
    writeFingerPref :: FingersMask -> X ()
    writeFingerPref pref = do
      liftIO . writeFile currentFingersPath $ show pref
      spawn
        [i|notify-send 'Making a preference was succeed, now you can replace xmonad!'|]

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

-- | See `setKeymapToUS`
data XKeyboardLayout
  = USKeyboardLayout
  | JPKeyboardLayout
  | ResetSetXKBMAP

asXkbmapStuff :: XKeyboardLayout -> String
asXkbmapStuff USKeyboardLayout = "us"
asXkbmapStuff JPKeyboardLayout = "jp"
asXkbmapStuff ResetSetXKBMAP   = "us" -- us by default

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
