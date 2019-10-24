module XMonadConfig.Keys.FingersMask where

import Data.Default (Default (..))
import XMonad

altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask

-- | Serializable `KeyMask` for known key masks
data KeyMask' = CapsLockMask
              | AltMask
              | SuperMask
              | ShiftMask
              | ControlMask
  deriving (Show, Read)

fromKeyMask' :: KeyMask' -> KeyMask
fromKeyMask' CapsLockMask = lockMask
fromKeyMask' AltMask      = altMask
fromKeyMask' SuperMask    = superMask
fromKeyMask' ShiftMask    = shiftMask
fromKeyMask' ControlMask  = controlMask

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

-- | NOTE: Implement new behavior that chooses default layout or something if layout changing needed
currentFingers :: FingersMask
currentFingers = def
