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
    getGeneratedServerDeps,
  )
import qualified Wasp.Validator as V

dependenciesValidator :: AS.AppSpec -> V.Validator P.PackageJson
dependenciesValidator spec =
  V.all
    [ optionalDepsValidator,
      forbiddenDepsValidator
    ]
  where
    -- npm checks the versions of @wasp/wasp's peer dependencies (react,
    -- vite, express, ...), so this only checks the generated server's.
    optionalDepsValidator :: V.Validator P.PackageJson
    optionalDepsValidator =
      V.all $
        [ makeOptionalDepValidator depType dep
        | depType <- [Runtime, Development],
          -- The user may list a server dependency in either field.
          dep <- M.toList $ getGeneratedServerDeps spec
        ]

    forbiddenDepsValidator :: V.Validator P.PackageJson
    forbiddenDepsValidator =
      V.all $
        [ makeForbiddenDepValidator depType pkgName
        | depType <- [Runtime, Development],
          pkgName <- forbiddenUserDeps
        ]
