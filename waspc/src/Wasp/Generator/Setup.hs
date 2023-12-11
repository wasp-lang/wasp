module Wasp.Generator.Setup
  ( runSetup,
  )
where

import Control.Monad (when)
import StrongPath (Abs, Dir, Path')
import Wasp.AppSpec (AppSpec (waspProjectDir))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.DbGenerator as DbGenerator
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import Wasp.Generator.NpmInstall (installNpmDependenciesWithInstallRecord, isNpmInstallNeeded)
import qualified Wasp.Message as Msg

runSetup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
runSetup spec dstDir sendMessage = do
  runNpmInstallIfNeeded spec dstDir sendMessage >>= \case
    npmInstallResults@(_, []) -> (npmInstallResults <>) <$> setUpDatabase spec dstDir sendMessage
    npmInstallResults -> return npmInstallResults

runNpmInstallIfNeeded :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
runNpmInstallIfNeeded spec dstDir sendMessage = do
  isNpmInstallNeeded spec dstDir >>= \case
    Left errorMessage -> return ([], [GenericGeneratorError errorMessage])
    Right maybeFullStackDeps -> case maybeFullStackDeps of
      Nothing -> return ([], [])
      Just fullStackDeps -> do
        sendMessage $ Msg.Start "Starting npm install..."
        (npmInstallWarnings, npmInstallErrors) <-
          installNpmDependenciesWithInstallRecord fullStackDeps (waspProjectDir spec) dstDir
        when (null npmInstallErrors) (sendMessage $ Msg.Success "Successfully completed npm install.")
        return (npmInstallWarnings, npmInstallErrors)

setUpDatabase :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
setUpDatabase spec dstDir sendMessage = do
  sendMessage $ Msg.Start "Setting up database..."
  (dbGeneratorWarnings, dbGeneratorErrors) <- DbGenerator.postWriteDbGeneratorActions spec dstDir
  when (null dbGeneratorErrors) (sendMessage $ Msg.Success "Database successfully set up.")
  return (dbGeneratorWarnings, dbGeneratorErrors)
