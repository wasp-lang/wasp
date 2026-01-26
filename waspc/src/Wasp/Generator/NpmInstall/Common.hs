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
import qualified Wasp.Generator.WebAppGenerator as WG

data AllNpmDeps = AllNpmDeps
  { _userNpmDeps :: !N.NpmDepsFromUser, -- Deps coming from user's package.json .
    _waspFrameworkNpmDeps :: !N.NpmDepsForFramework, -- Deps coming from Wasp's framework code (webapp, server) package.jsons.
    _waspSdkNpmDeps :: !N.NpmDepsForPackage -- Deps coming from Wasp's SDK's package.json .
  }
  deriving (Eq, Show, Generic)

instance ToJSON AllNpmDeps

instance FromJSON AllNpmDeps

getAllNpmDeps :: AppSpec -> AllNpmDeps
getAllNpmDeps spec =
  AllNpmDeps
    { _userNpmDeps =
        N.getUserNpmDepsForPackage spec,
      _waspFrameworkNpmDeps =
        N.buildWaspFrameworkNpmDeps (SG.npmDepsFromWasp spec) (WG.npmDepsFromWasp spec),
      _waspSdkNpmDeps =
        SdkGenerator.npmDepsForSdk spec
    }
