module Wasp.Generator.Valid (validateAppSpec) where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Monad (GeneratorError)
import Wasp.Generator.Valid.PackageJson (validatePackageJson)

validateAppSpec :: AppSpec -> [GeneratorError]
validateAppSpec spec =
  validatePackageJson spec (AS.packageJson spec)
