{-# LANGUAGE Rank2Types #-}

module XMonadConfig.Types
  ( WorkspaceName
  , Workspaces
  , FilePath'
  ) where

import Data.String (IsString)

-- | The another expression of XMonad.Config.XConfig.workspaces
type Workspaces    = [WorkspaceName]
type WorkspaceName = String


-- | Polymorphic string
type FilePath' = forall s. IsString s => s
