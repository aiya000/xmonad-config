import Data.Monoid (All)
import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.EZConfig (additionalMouseBindings)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonadConfig.Keys (myKeys, superMask, altMask)
import XMonadConfig.LayoutHook (myLayoutHook)
import XMonadConfig.XConfig (myTerminal, myWorkspaces)

main :: IO ()
main =
  xmonad . ewmh $ desktopConfig
    { terminal           = myTerminal
    , modMask            = superMask
    , keys               = myKeys
    , borderWidth        = 2
    , layoutHook         = myLayoutHook
    , startupHook        = myStartupHook
    , manageHook         = myManageHook
    , workspaces         = myWorkspaces
    , focusFollowsMouse  = False
    , focusedBorderColor = "#0000ff"
    , handleEventHook    = myHandleEventHook
    }
    `additionalMouseBindings` myMouseBindings


myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D" -- Fix to start of Java Swing apps
  spawnOnce myTerminal

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Xfce4-panel" --> doIgnore
  , className =? "Xfdesktop"   --> doIgnore
  , className =? "io.github.aiya000.DoromochiApp" --> doFloat
  ]

myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
  [ ((altMask .|. controlMask, button1), mouseResizeWindow)
  , ((shiftMask .|. controlMask, button1), mouseMoveWindow)
  ]

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def <+> fullscreenEventHook
