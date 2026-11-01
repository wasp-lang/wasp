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
  )
import Wasp.Generator.Valid.PackageJson.Common
  ( forbiddenUserDeps,
    getAllWaspDependencies,
  )
import qualified Wasp.Validator as V

dependenciesValidator :: AS.AppSpec -> V.Validator P.PackageJson
dependenciesValidator spec =
  V.all
    [ optionalDepsValidator,
      forbiddenDepsValidator
    ]
  where
    -- The peer dependencies of @wasp/wasp (react, vite, express, ...) aren't
    -- checked here, npm enforces their versions.
    optionalDepsValidator :: V.Validator P.PackageJson
    optionalDepsValidator =
      V.all $
        [ makeOptionalDepValidator depType dep
        | depType <- [Runtime, Development],
          -- We check all the Wasp dependencies everywhere in case they are used
          -- in the user's package.json, to avoid conflicts.
          dep <- M.toList $ getAllWaspDependencies spec
        ]

    forbiddenDepsValidator :: V.Validator P.PackageJson
    forbiddenDepsValidator =
      V.all $
        [ makeForbiddenDepValidator depType pkgName
        | depType <- [Runtime, Development],
          pkgName <- forbiddenUserDeps
        ]
