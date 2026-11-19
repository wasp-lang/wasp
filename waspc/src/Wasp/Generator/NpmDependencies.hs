{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | TODO(#4824): Only the install after compile uses this module, to notice
-- that the user's package.json changed. It goes away with that install.
module Wasp.Generator.NpmDependencies
  ( getUserNpmDepsForPackage,
    NpmDepsForPackage (..),
    NpmDepsFromUser (..),
  )
where

import Data.Aeson
import Data.List (sort)
import GHC.Generics
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ

data NpmDepsForPackage = NpmDepsForPackage
  { dependencies :: [D.Dependency],
    devDependencies :: [D.Dependency]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype NpmDepsFromUser = NpmDepsFromUser {fromUser :: NpmDepsForPackage}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

getUserNpmDepsForPackage :: AppSpec -> NpmDepsFromUser
getUserNpmDepsForPackage spec =
  NpmDepsFromUser $
    NpmDepsForPackage
      { dependencies = PJ.getDependencies $ AS.packageJson spec,
        -- Should we allow user devDependencies? https://github.com/wasp-lang/wasp/issues/456
        devDependencies = PJ.getDevDependencies $ AS.packageJson spec
      }

-- NpmDepsForPackage are equal if their sorted dependencies
-- are equal.
instance Eq NpmDepsForPackage where
  (==) a b = sortedDependencies a == sortedDependencies b

sortedDependencies :: NpmDepsForPackage -> ([D.Dependency], [D.Dependency])
sortedDependencies a = (sort $ dependencies a, sort $ devDependencies a)
