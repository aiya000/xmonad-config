-- | For another `XConfig` terms
module XMonadConfig.XConfig
  ( myTerminal
  , myWebBrowser
  , myWorkspaces
  ) where


myTerminal :: String
myTerminal = "urxvt"

myWebBrowser :: String
myWebBrowser = "vivaldi-stable"

myWorkspaces :: [String]
myWorkspaces = map show [1..4]
