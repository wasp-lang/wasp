{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.NpmInstall.Common
  ( AllNpmDeps (..),
    getAllNpmDeps,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.Generator.NpmDependencies as N

-- | TODO(#4824): The generated code lists no dependencies anymore, so the
-- install after compile and this record of it go away.
newtype AllNpmDeps = AllNpmDeps
  { _userNpmDeps :: N.NpmDepsFromUser -- Deps coming from user's package.json .
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

getAllNpmDeps :: AppSpec -> AllNpmDeps
getAllNpmDeps spec =
  AllNpmDeps
    { _userNpmDeps = N.getUserNpmDepsForPackage spec
    }
