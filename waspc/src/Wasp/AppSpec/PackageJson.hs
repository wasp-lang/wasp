{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasp.AppSpec.PackageJson where

import Control.Applicative (liftA2)
import Data.Aeson.TH
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Wasp.AppSpec.App.Dependency (Dependency)
import qualified Wasp.AppSpec.App.Dependency as D

data PackageJson = PackageJson
  { _name :: !String,
    -- todo(filip): do this properly once you merge martin's PR
    _dependencies :: !(Map String String),
    _devDependencies :: !(Map String String)
  }
  deriving (Show, Generic)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PackageJson)

dependencies :: PackageJson -> [Dependency]
dependencies packageJson = D.fromList $ M.toList $ _dependencies packageJson

devDependencies :: PackageJson -> [Dependency]
devDependencies packageJson = D.fromList $ M.toList $ _devDependencies packageJson

allDependencies :: PackageJson -> [Dependency]
allDependencies = liftA2 (++) dependencies devDependencies
