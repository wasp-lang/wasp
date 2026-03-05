module Wasp.Generator.Valid.PackageJson
  ( validatePackageJson,
  )
where

import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.Valid.PackageJson.Dependencies (dependenciesValidator)
import Wasp.Generator.Valid.PackageJson.WaspConfig (waspConfigValidator)
import Wasp.Generator.Valid.PackageJson.Workspaces (workspacesValidator)
import qualified Wasp.Generator.Valid.Validator as V

validatePackageJson :: AS.AppSpec -> P.PackageJson -> [GeneratorError]
validatePackageJson spec pkgJson =
  GenericGeneratorError . show
    <$> V.execValidator packageJsonValidator pkgJson
  where
    packageJsonValidator :: V.Validator P.PackageJson
    packageJsonValidator =
      V.withFileName "package.json" $
        V.all
          [ workspacesValidator,
            waspConfigValidator spec,
            dependenciesValidator spec
          ]
