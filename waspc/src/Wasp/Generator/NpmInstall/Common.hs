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
import qualified Wasp.Generator.SdkGenerator as SdkG
import qualified Wasp.Generator.ServerGenerator as ServerG
import qualified Wasp.Generator.WebAppGenerator as WebAppG

data AllNpmDeps = AllNpmDeps
  { _userNpmDeps :: !N.NpmDepsFromUser, -- Deps coming from user's package.json .
    _waspFrameworkNpmDeps :: !N.NpmDepsForFramework, -- Deps coming from Wasp's framework code (webapp, server) package.jsons.
    _waspSdkNpmDeps :: !N.NpmDepsForPackage -- Deps coming from Wasp's SDK's package.json .
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

getAllNpmDeps :: AppSpec -> AllNpmDeps
getAllNpmDeps spec =
  AllNpmDeps
    { _userNpmDeps =
        N.getUserNpmDepsForPackage spec,
      _waspFrameworkNpmDeps =
        N.buildWaspFrameworkNpmDeps (ServerG.npmDepsFromWasp spec) (WebAppG.npmDepsFromWasp spec),
      _waspSdkNpmDeps =
        SdkG.npmDepsForSdk spec
    }
