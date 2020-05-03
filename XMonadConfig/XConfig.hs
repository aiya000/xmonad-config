-- | For another `XConfig` terms
module XMonadConfig.XConfig
  ( myTerminal
  , myWebBrowser
  , myWorkspaces
  ) where

myTerminal :: String
myTerminal = "xfce4-terminal"

myWebBrowser :: String
myWebBrowser = "vivaldi-stable"

-- NOTE: 1 is hard to type.
myWorkspaces :: [String]
myWorkspaces = map show [2 .. 5]
