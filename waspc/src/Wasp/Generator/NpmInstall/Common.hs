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
  deriving (Eq, Show, Generic)

instance ToJSON AllNpmDeps

instance FromJSON AllNpmDeps

getAllNpmDeps :: AppSpec -> Either String AllNpmDeps
getAllNpmDeps spec =
  let userNpmDeps = N.getUserNpmDepsForPackage spec
      errorOrWaspServerNpmDeps =
        N.buildWaspServerNpmDeps spec (SG.npmDepsFromWasp spec)
      waspSdkNpmDeps = SdkGenerator.npmDepsForSdk spec
   in case errorOrWaspServerNpmDeps of
        Left message -> Left $ "determining npm deps to install failed: " ++ message
        Right waspServerNpmDeps ->
          Right $
            AllNpmDeps
              { _userNpmDeps = userNpmDeps,
                _waspServerNpmDeps = waspServerNpmDeps,
                _waspSdkNpmDeps = waspSdkNpmDeps
              }
