module Wasp.Generator.Setup
  ( runSetup,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.DbGenerator as DbGenerator
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import Wasp.Generator.NpmInstall (ensureNpmInstall)

runSetup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO ([GeneratorWarning], [GeneratorError])
runSetup spec dstDir = do
  -- TODO: Use a monad transformer here in the future for better shortcircuiting behavior.
  (npmInstallWarnings, npmInstallErrors) <- ensureNpmInstall spec dstDir
  if null npmInstallErrors
    then do
      (dbGeneratorWarnings, dbGeneratorErrors) <- DbGenerator.postWriteDbGeneratorActions spec dstDir
      return (npmInstallWarnings ++ dbGeneratorWarnings, dbGeneratorErrors)
    else return (npmInstallWarnings, npmInstallErrors)
