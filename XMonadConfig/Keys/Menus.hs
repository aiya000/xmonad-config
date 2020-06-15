{-# LANGUAGE QuasiQuotes #-}

module XMonadConfig.Keys.Menus where

import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString (..))
import Data.String.Here (i)
import Data.Text (Text)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import XMonad
import XMonad.Prompt (XPConfig (..), XPPosition (..), greenXPConfig)
import XMonad.Prompt.Input (inputPromptWithCompl, (?+))

default (Text)

-- | Polymorphic string
type FilePath' = forall s. IsString s => s

-- |
-- Shortcut for @$HOME@ .
-- Apply @$HOME@ to @f@
withHomeDir :: MonadIO m => (FilePath' -> m a) -> m a
withHomeDir f = do
  homeDir <- liftIO $ getEnv "HOME"
  f $ fromString homeDir

myXPConf :: XPConfig
myXPConf = greenXPConfig
  { font = "xft:Ricty:Regular:size=10:antialias=true"
  , position = Top
  }

-- | Load a .xmodmap
xmodmapMenu :: X ()
xmodmapMenu =
  inputPromptWithCompl myXPConf "Load a .xmodmap" xmonadXmodmaps ?+ \xmodmapFile ->
    spawn
      [i| -- setxkbmap must be done before xmodmap, because setxkbmap overwrites.
      setxkbmap -layout us -option caps:ctrl_modifier &&
      xmodmap ~/.xmonad/Xmodmap/${xmodmapFile} &&
      notify-send '${xmodmapFile} did seem to be loaded :D'
    |]
    -- ~/.xmonad/Xmodmap/*
  where
    xmonadXmodmaps _ = withHomeDir $
      (filter (/= "README.md") <$>) . listDirectory . (<> "/.xmonad/Xmodmap")
