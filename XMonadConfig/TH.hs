-- | For the 'GHC stage restriction' of TemplateHaskell
module XMonadConfig.TH where

import Data.Semigroup ((<>))
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)

-- |
-- A file path of ~/.xmonad/currentFingers and ~/.xmonad/currentFingers-default.
-- This is for only compile time processings,
-- because this uses 'unsafePerformIO' internally.
currentFingersPaths :: [FilePath]
currentFingersPaths =
  let homeDir = unsafePerformIO $ getEnv "HOME"
  in map (homeDir <>) ["/.xmonad/currentFingers", "/.xmonad/currentFingers-default"]

-- |
-- Similar to 'currentFingersPaths', only ~/.xmonad/currentFingers
currentFingersPath :: FilePath
currentFingersPath =
  let homeDir = unsafePerformIO $ getEnv "HOME"
  in homeDir <> "/.xmonad/currentFingers"
