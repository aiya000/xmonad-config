#!/usr/bin/env stack
-- stack --resolver lts-13.22 --install-ghc runghc --package shelly --package text --package easy-file --package here --package safe-exceptions

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Exception.Safe (SomeException, handle)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.String.Here (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Shelly (Sh, shelly, verbosely, (-|-))
import qualified Shelly as Sh
import System.EasyFile (getCurrentDirectory, setCurrentDirectory)
import System.Exit (exitFailure)

default (Text)

main :: IO ()
main = do
  xmonadDir <- getCurrentDirectory
  let exit'  = exit xmonadDir
  let start' = start xmonadDir
  handle exit' start'
  where
    exit :: FilePath -> SomeException -> IO a
    exit xmonadDir e = do
      putStrLn [i|${xmonadDir} is not found (${e})|]
      exitFailure

    start xmonadDir = do
      setCurrentDirectory xmonadDir
      putStrLn [i|${xmonadDir}/replace starts|]
      shelly . verbosely $ replace xmonadDir

replace :: FilePath -> Sh ()
replace xmonadDir = handle (exit xmonadDir) $ do
  stdout <- Text.unlines <$> sequence tasks
  liftIO $ Text.writeFile [i|${xmonadDir}/xmonad-config.log|] stdout
  where
    tasks =
      [ Sh.run "stack" ["clean"] -- clean to load `currentFingers`
      , Sh.run "stack" ["install"]
      , Sh.run "stack" ["exec", "--", "xmonad-config", "--recompile"]
      , Sh.run "stack" ["exec", "--", "xmonad-config", "--restart"]
      ]

    exit :: FilePath -> SomeException -> Sh a
    exit xmonadDir e = liftIO $ do
      writeFile [i|${xmonadDir}/xmonad-config.log|] $ show e
      exitFailure
