module Wasp.Generator.Valid (validateExternalConfigsWithAppSpec) where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Monad (GeneratorError)
import Wasp.Generator.Valid.PackageJson (validatePackageJson)

validateExternalConfigsWithAppSpec :: AppSpec -> [GeneratorError]
validateExternalConfigsWithAppSpec spec =
  validatePackageJson spec (AS.packageJson spec)
