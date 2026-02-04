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
import qualified Wasp.Generator.SdkGenerator as SdkGenerator
import qualified Wasp.Generator.ServerGenerator as SG

data AllNpmDeps = AllNpmDeps
  { _userNpmDeps :: !N.NpmDepsFromUser, -- Deps coming from user's package.json .
    _waspServerNpmDeps :: !N.NpmDepsForPackage, -- Deps coming from Wasp's server package.json.
    _waspSdkNpmDeps :: !N.NpmDepsForPackage -- Deps coming from Wasp's SDK's package.json .
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

getAllNpmDeps :: AppSpec -> AllNpmDeps
getAllNpmDeps spec =
  AllNpmDeps
    { _userNpmDeps =
        N.getUserNpmDepsForPackage spec,
      _waspServerNpmDeps =
        N.buildWaspServerNpmDeps spec (SG.npmDepsFromWasp spec),
      _waspSdkNpmDeps =
        SdkGenerator.npmDepsForSdk spec
    }
