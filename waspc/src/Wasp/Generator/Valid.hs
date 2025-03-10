module Wasp.Generator.Valid where

import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Monad (GeneratorError)
import Wasp.Generator.Valid.PackageJson (validatePackageJson)
import Wasp.Generator.Valid.TsConfig (validateSrcTsConfig)

validateAppSpec :: AppSpec -> [GeneratorError]
validateAppSpec spec =
  concat
    [ validatePackageJson spec,
      validateSrcTsConfig spec
    ]
