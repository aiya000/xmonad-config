{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module XMonadConfig.Keys.FingersMask where

import Data.Default (Default (..))
import Data.FileEmbed (embedOneFileOf)
import Data.Maybe (fromMaybe)
import Data.String.Here (i)
import Safe (readMay)
import XMonad
import XMonadConfig.TH (currentFingersPath, currentFingersPaths)
import qualified Data.ByteString.Char8 as ByteString

altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask

-- | Serializable `KeyMask` for known key masks
data KeyMask' = AltMask
              | SuperMask
              | ShiftMask
              | ControlMask
  deriving (Show, Read)

fromKeyMask' :: KeyMask' -> KeyMask
fromKeyMask' AltMask     = altMask
fromKeyMask' SuperMask   = superMask
fromKeyMask' ShiftMask   = shiftMask
fromKeyMask' ControlMask = controlMask

-- | A serializable set of `KeyMask'`
data FingersMask = FingersMask
  { ringMask'   :: KeyMask' -- ^ the ring (finger) mask
  , littleMask' :: KeyMask' -- ^ the little (finger) mask
  , thumbMask'  :: KeyMask' -- ^ the thumb mask
  } deriving (Show, Read)

fromFingersMask :: FingersMask -> (KeyMask, KeyMask, KeyMask)
fromFingersMask FingersMask {..} =
  ( fromKeyMask' ringMask'
  , fromKeyMask' littleMask'
  , fromKeyMask' thumbMask'
  )

-- | A finger masks layout for surface type cover
instance Default FingersMask where
  def = FingersMask
    { ringMask' = SuperMask
    , littleMask' = ControlMask
    , thumbMask' = AltMask
    }

-- | A 'FingersMask' layout for HHKB Lite2 us keyboard
hhkbLiteFamilyFingers :: FingersMask
hhkbLiteFamilyFingers = FingersMask
  { ringMask' = AltMask
  , littleMask' = ControlMask
  , thumbMask' = SuperMask
  }

-- |
-- On the compile time,
-- load currentFingers file if it is existent,
-- or Load currentFingers-default (usually, currentFingers-default is existent),
-- or use `Default` of `FingersMask`.
currentFingers :: FingersMask
currentFingers = fromMaybe def . readMay $
  ByteString.unpack $(embedOneFileOf currentFingersPaths)

-- | Save!
writeFingerPref :: FingersMask -> X ()
writeFingerPref pref = do
  liftIO . writeFile currentFingersPath $ show pref
  spawn [i|notify-send 'Making a preference was succeed, now you can replace xmonad!'|]
