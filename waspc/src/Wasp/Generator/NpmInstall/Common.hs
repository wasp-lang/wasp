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
import qualified Wasp.Generator.ServerGenerator as SG
import qualified Wasp.Generator.WebAppGenerator as WG

data AllNpmDeps = AllNpmDeps
  { _userNpmDeps :: !N.NpmDepsForUser, -- Deps coming from user's package.json .
    _waspNpmDeps :: !N.NpmDepsForFullStack -- Deps coming from Wasp's framework code (webapp, server) package.jsons.
  }
  deriving (Eq, Show, Generic)

instance ToJSON AllNpmDeps

instance FromJSON AllNpmDeps

getAllNpmDeps :: AppSpec -> Either String AllNpmDeps
getAllNpmDeps spec =
  let userNpmDeps = N.getUserNpmDepsForPackage spec
      errorOrWaspNpmDeps = N.buildWaspNpmDepsForFullStack spec (SG.npmDepsForWasp spec) (WG.npmDepsForWasp spec)
   in case errorOrWaspNpmDeps of
        Left message -> Left $ "determining npm deps to install failed: " ++ message
        Right waspNpmDeps -> Right $ AllNpmDeps {_userNpmDeps = userNpmDeps, _waspNpmDeps = waspNpmDeps}
