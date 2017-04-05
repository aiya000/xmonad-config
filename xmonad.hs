import Control.Monad ((>=>), void)
import Control.Monad.Extra (ifM)
import Data.Map.Lazy (Map)
import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.FloatKeys (keysMoveWindow)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.Volume (toggleMute, lowerVolume, raiseVolume)
import XMonad.Config.Kde (kdeConfig)
import XMonad.Hooks.Place (placeHook, fixed)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout (ChangeLayout(FirstLayout,NextLayout))
import XMonad.Layout.Gaps (gaps, Direction2D(U))
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.SubLayouts (subTabbed, GroupMsg(MergeAll,UnMerge))
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.TwoPane (TwoPane(TwoPane))
import XMonad.Operations (sendMessage, withFocused, mouseResizeWindow)
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown, greedyView, shift)
import XMonad.Util.EZConfig (additionalMouseBindings)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Types (Direction2D(..))
import XMonadConfig.CommandWrapper (takeScreenShot)
import XMonadConfig.Shelly (switchKeyModeTo, currentKeyModeIs)
import qualified Data.Map.Lazy as M
import qualified XMonadConfig.CommandWrapper as CW
import qualified XMonadConfig.Shelly as SH


main :: IO ()
main = do
  inUnixKeymapMode <- currentKeyModeIs SH.UnixKeymap
  let (myModMask, myKeys) = if inUnixKeymapMode then (unixCasualMask, myUnixKeys)
                                                else (superMask, myNormalKeys)
  xmonad $ kdeConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , keys               = myKeys
    , borderWidth        = 2
    , layoutHook         = myLayoutHook
    , startupHook        = myStartupHook
    , manageHook         = manageHook kdeConfig <+> myManageHook
    , workspaces         = myWorkspaces
    , focusFollowsMouse  = False
    , focusedBorderColor = "#0000ff"
    }
    `additionalMouseBindings` myMouseBindings


altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask

unixCasualMask :: KeyMask
unixCasualMask = controlMask .|. shiftMask


myTerminal :: String
myTerminal = "termite"

myLayoutHook = kdeTaskbarMargin $ twoTabbedPane ||| Grid
  where
    kdeTaskbarMargin = gaps [(D, 40)]
    twoTabbedPane    = subTabbed $ TwoPane (1/55) (1/2)

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"  -- For Java Swing apps starting

myManageHook :: ManageHook
myManageHook = placeHook (fixed (0.5, 0.5)) <+> manageFloatForTargets
  where
    manageFloatForTargets = composeAll
      [ className =? "plasmashell" --> doIgnore
      ]

myWorkspaces :: [String]
myWorkspaces = map show [1..4]


myNormalKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myNormalKeys _ = M.fromList $
  [ ((altMask .|. controlMask, xK_h), windows swapUp)
  , ((altMask .|. controlMask, xK_l), windows swapDown)
  , ((altMask .|. controlMask, xK_i), nextScreen)
  , ((altMask .|. controlMask, xK_a), sinkAll)
  , ((altMask .|. controlMask, xK_n), sendMessage NextLayout)
  , ((altMask, xK_h), windows focusUp)
  , ((altMask, xK_j), withFocused $ sendMessage . MergeAll)
  , ((altMask, xK_k), withFocused $ sendMessage . UnMerge)
  , ((altMask, xK_l), windows focusDown)
  , ((superMask, xK_F1), spawn "light -U 10")
  , ((superMask, xK_F2), spawn "light -A 10")
  , ((superMask, xK_F4), void $ toggleMute)
  , ((superMask, xK_F5), void $ lowerVolume 5)
  , ((superMask, xK_F6), void $ raiseVolume 5)
  , ((superMask, xK_F10), CW.lockScreen)
  , ((superMask, xK_F11), CW.lockScreenSuspend)
  , ((superMask, xK_F12), CW.lockScreenHibernate)
  , ((superMask, xK_e), spawn "thunar")
  , ((superMask, xK_f), spawn "firefox")
  , ((superMask, xK_h), withFocused $ keysMoveWindow (-5,0))
  , ((superMask, xK_j), withFocused $ keysMoveWindow (0,5))
  , ((superMask, xK_k), withFocused $ keysMoveWindow (0,-5))
  , ((superMask, xK_l), withFocused $ keysMoveWindow (5,0))
  , ((superMask, xK_m), spawn "xfce4-mixer")
  , ((superMask, xK_r), spawn "dmenu_run")
  , ((superMask, xK_t), spawn myTerminal)
  -- Another KeyMask
  , ((noModMask, xK_Print), takeScreenShot CW.FullScreen)
  , ((shiftMask, xK_Print), takeScreenShot CW.ActiveWindow)
  , ((unixCasualMask, xK_x), switchKeyModeTo SH.UnixKeymap)
  ]
  -- Move current window to target worskpace
  ++ [((altMask, numKey), windows . shift $ workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces ]

myUnixKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myUnixKeys _ = M.fromList $
  [ ((unixCasualMask .|. altMask, xK_h), windows swapUp)
  , ((unixCasualMask .|. altMask, xK_l), windows swapDown)
  , ((unixCasualMask, xK_a), sinkAll)
  , ((unixCasualMask, xK_c), kill)
  , ((unixCasualMask, xK_e), spawn "thunar")
  , ((unixCasualMask, xK_f), spawn "firefox")
  , ((unixCasualMask, xK_g), sendMessage NextLayout)
  , ((unixCasualMask, xK_h), windows focusUp)
  , ((unixCasualMask, xK_i), nextScreen)
  , ((unixCasualMask, xK_j), withFocused $ sendMessage . MergeAll)
  , ((unixCasualMask, xK_k), withFocused $ sendMessage . UnMerge)
  , ((unixCasualMask, xK_l), windows focusDown)
  , ((unixCasualMask, xK_m), spawn "xfce4-mixer")
  , ((unixCasualMask, xK_r), spawn "dmenu_run")
  , ((unixCasualMask, xK_t), spawn myTerminal)
  , ((unixCasualMask, xK_x), switchKeyModeTo SH.Common)
  , ((noModMask, xK_Print), takeScreenShot CW.FullScreen)
  , ((shiftMask, xK_Print), takeScreenShot CW.ActiveWindow)
  , ((superMask, xK_F10), CW.lockScreen)
  , ((superMask, xK_F11), CW.lockScreenSuspend)
  , ((superMask, xK_F12), CW.lockScreenHibernate)
  , ((superMask, xK_F1), spawn "light -U 10")
  , ((superMask, xK_F2), spawn "light -A 10")
  , ((superMask, xK_F3), void $ lowerVolume 5)
  , ((superMask, xK_F4), void $ raiseVolume 5)
  , ((superMask, xK_Print), takeScreenShot CW.FullScreen)
  ]
  -- Move current window to target worskpace
  ++ [ ((unixCasualMask, numKey), windows . greedyView $ workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces ]


myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
  [ ((altMask, button1), mouseResizeWindow)
  ]
