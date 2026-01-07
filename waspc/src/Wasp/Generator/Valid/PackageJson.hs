module Wasp.Generator.Valid.PackageJson
  ( validatePackageJson,
  )
where

import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.Valid.PackageJson.Dependencies (dependenciesValidator)
import Wasp.Generator.Valid.PackageJson.Workspaces (workspacesValidator)
import qualified Wasp.Generator.Valid.Validator as V

validatePackageJson :: P.PackageJson -> [GeneratorError]
validatePackageJson pkgJson =
  GenericGeneratorError . show
    <$> V.execValidator packageJsonValidator pkgJson
  where
    packageJsonValidator :: V.Validator P.PackageJson
    packageJsonValidator =
      V.withFileName "package.json" $
        V.all
          [ workspacesValidator,
            dependenciesValidator
          ]
