module Wasp.Generator.Valid (validateAppSpec) where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Monad (GeneratorError)
import qualified Wasp.Generator.TailwindConfigFile as TCF
import Wasp.Generator.Valid.PackageJson (validatePackageJson)
import qualified Wasp.Generator.Valid.PackageJsonValidationContext as C
import Wasp.Generator.Valid.TsConfig (validateSrcTsConfig)

validateAppSpec :: AppSpec -> [GeneratorError]
validateAppSpec spec =
  concat
    [ validatePackageJson (AS.packageJson spec) packageJsonValidationContext,
      validateSrcTsConfig (AS.srcTsConfig spec)
    ]
  where
    packageJsonValidationContext =
      C.PackageJsonValidationContext
        { C.isTailwindUsed = TCF.isTailwindUsed spec
        }
