-- | For another `XConfig` terms
module XMonadConfig.XConfig
  ( myTerminal
  , myWebBrowser
  , myWorkspaces
  ) where


myTerminal :: String
myTerminal = "termite"

myWebBrowser :: String
myWebBrowser = "firefox"

myWorkspaces :: [String]
myWorkspaces = map show [1..4]
