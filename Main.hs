import Data.Default (def)
import Data.Monoid (All)
import XMonad
import XMonad.Config.Xfce (xfceConfig)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsEventHook, ewmhDesktopsStartup, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.EZConfig (additionalMouseBindings)
import XMonadConfig.Keys (myKeys)
import XMonadConfig.Keys.FingersMask (superMask)
import XMonadConfig.LayoutHook (myLayoutHook)
import XMonadConfig.XConfig (myTerminal, myWorkspaces)

main :: IO ()
main = xmonad . docks $ xfceConfig
  { terminal = myTerminal
  , modMask = superMask
  , keys = myKeys def
  , layoutHook = myLayoutHook
  , startupHook = myStartupHook
  , manageHook = myManageHook
  , workspaces = myWorkspaces
  , focusFollowsMouse = False
  , focusedBorderColor = "#006400"
  , borderWidth = 4
  , handleEventHook = myHandleEventHook
  }
  `additionalMouseBindings` myMouseBindings


myStartupHook :: X ()
myStartupHook = do
  ewmhDesktopsStartup
  setWMName "LG3D" -- Fix startings of Java Swing apps
  spawn "xfce4-panel"
  spawn myTerminal
  spawn "autokey-gtk --configure"

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Xfce4-panel" --> doIgnore
  , className =? "Xfdesktop" --> doIgnore
  , className =? "io.github.aiya000.DoromochiApp" --> doFloat
  ]

myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
  [ ((shiftMask .|. superMask, button1), mouseResizeWindow)
  , ((controlMask .|. superMask, button1), mouseMoveWindow)
  ]

myHandleEventHook :: Event -> X All
myHandleEventHook =
  handleEventHook def <+>
  fullscreenEventHook <+>
  ewmhDesktopsEventHook
