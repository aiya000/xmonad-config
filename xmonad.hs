import           Control.Monad                 ((>=>))
import           Data.Monoid                   (All)
import           XMonad
import           XMonad.Config.Desktop         (desktopConfig)
import           XMonad.Hooks.DynamicLog       (dzenPP, statusBar)
import           XMonad.Hooks.EwmhDesktops     (ewmhDesktopsEventHook,
                                                ewmhDesktopsStartup,
                                                fullscreenEventHook)
import           XMonad.Hooks.ManageDocks      (AvoidStruts)
import           XMonad.Hooks.SetWMName        (setWMName)
import           XMonad.Layout.LayoutModifier  (ModifiedLayout)
import           XMonad.Util.EZConfig          (additionalMouseBindings)
import           XMonadConfig.Keys             (myKeys, myToggleStrutsKey)
import           XMonadConfig.Keys.FingersMask (altMask, superMask)
import           XMonadConfig.LayoutHook       (myLayoutHook)
import           XMonadConfig.XConfig          (myTerminal, myWorkspaces)

main :: IO ()
main =
  dzen >=> xmonad $
  desktopConfig
    { terminal = myTerminal
    , modMask = superMask
    , keys = myKeys
    , layoutHook = myLayoutHook
    , startupHook = myStartupHook
    , manageHook = myManageHook
    , workspaces = myWorkspaces
    , focusFollowsMouse = False
    , focusedBorderColor = "#006400"
    , borderWidth = 4
    , handleEventHook = myHandleEventHook
    } `additionalMouseBindings`
  myMouseBindings

-- | Run dummy dzen2 for 'myToggleStrutsKey', please see ~/.zshrc_env for real dzen2 startup
dzen ::
     LayoutClass l Window
  => XConfig l
  -> IO (XConfig (ModifiedLayout AvoidStruts l))
dzen = statusBar "echo 'hi' | dzen2 -dock " dzenPP myToggleStrutsKey

myStartupHook :: X ()
myStartupHook = do
  ewmhDesktopsStartup
  setWMName "LG3D" -- Fix startings of Java Swing apps

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Xfce4-panel" --> doIgnore
    , className =? "Xfdesktop" --> doIgnore
    , className =? "io.github.aiya000.DoromochiApp" --> doFloat
    ]

myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
  [ ((controlMask .|. shiftMask .|. superMask, button1), mouseResizeWindow)
  , ((controlMask .|. shiftMask .|. altMask, button1), mouseMoveWindow)
  ]

myHandleEventHook :: Event -> X All
myHandleEventHook =
  handleEventHook def <+> fullscreenEventHook <+> ewmhDesktopsEventHook
