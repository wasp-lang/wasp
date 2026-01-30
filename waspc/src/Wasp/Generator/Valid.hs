module Wasp.Generator.Valid (validateAppSpec) where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Monad (GeneratorError)
import Wasp.Generator.Valid.PackageJson (validatePackageJson)
import Wasp.Generator.Valid.TsConfig (validateSrcTsConfig)

validateAppSpec :: AppSpec -> [GeneratorError]
validateAppSpec spec =
  concat
    [ validatePackageJson spec (AS.packageJson spec),
      validateSrcTsConfig (AS.srcTsConfig spec)
    ]
