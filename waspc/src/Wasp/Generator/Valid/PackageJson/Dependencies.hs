module Wasp.Generator.Valid.PackageJson.Dependencies
  ( dependenciesValidator,
  )
where

import qualified Data.Map as M
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.ExternalConfig.Npm.PackageJson.DepValidators
  ( DependencyType (..),
    makeForbiddenDepValidator,
    makeOptionalDepValidator,
    makeRequiredDepValidator,
  )
import Wasp.Generator.Valid.PackageJson.Common
  ( forbiddenUserDeps,
    getAllWaspDependencies,
    requiredUserDevelopmentDeps,
    requiredUserRuntimeDeps,
  )
import qualified Wasp.Validator as V

dependenciesValidator :: AS.AppSpec -> V.Validator P.PackageJson
dependenciesValidator spec =
  V.all
    [ runtimeDepsValidator,
      developmentDepsValidator,
      optionalDepsValidator,
      forbiddenDepsValidator
    ]
  where
    runtimeDepsValidator :: V.Validator P.PackageJson
    runtimeDepsValidator =
      V.all $ makeRequiredDepValidator Runtime <$> requiredUserRuntimeDeps

    developmentDepsValidator :: V.Validator P.PackageJson
    developmentDepsValidator =
      V.all $ makeRequiredDepValidator Development <$> requiredUserDevelopmentDeps

    optionalDepsValidator :: V.Validator P.PackageJson
    optionalDepsValidator =
      V.all $
        [ makeOptionalDepValidator depType dep
          | depType <- [Runtime, Development],
            -- We check all the Wasp dependencies everywhere in case they are used
            -- in the user's package.json, to avoid conflicts.
            dep <- M.toList onlyOptionalWaspDeps
        ]

    -- We don't check for dependencies as optional that are already going to be
    -- checked as required, to avoid redundant error messages.
    onlyOptionalWaspDeps = foldr (M.delete . fst) allWaspDeps allRequiredUserDeps
    allWaspDeps = getAllWaspDependencies spec
    allRequiredUserDeps = requiredUserRuntimeDeps <> requiredUserDevelopmentDeps

    forbiddenDepsValidator :: V.Validator P.PackageJson
    forbiddenDepsValidator =
      V.all $
        [ makeForbiddenDepValidator depType pkgName
          | depType <- [Runtime, Development],
            pkgName <- forbiddenUserDeps
        ]
