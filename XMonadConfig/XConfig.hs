-- | For another `XConfig` terms
module XMonadConfig.XConfig
  ( myTerminal
  , myWebBrowser
  , myWorkspaces
  ) where

myTerminal :: String
myTerminal = "termite"

myWebBrowser :: String
myWebBrowser = "vivaldi-stable"

-- NOTE: 1 is omitted because it is hard to type
myWorkspaces :: [String]
myWorkspaces = map show [2 .. 4]
