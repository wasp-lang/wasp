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
import qualified Wasp.Generator.SdkGenerator as SdkGenerator
import qualified Wasp.Message as Msg

runSetup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
runSetup spec projectRootDir sendMessage = do
  runNpmInstallIfNeeded spec projectRootDir sendMessage >>= \case
    npmInstallResults@(_, []) ->
      setUpDatabase spec projectRootDir sendMessage >>= \case
        setUpDatabaseResults@(_, []) -> do
          -- todo(filip): Should we consider building SDK as part of code generation?
          -- todo(filip): Avoid building on each setup if we don't need to.
          buildsSdkResults <- buildSdk projectRootDir sendMessage
          return $ npmInstallResults <> setUpDatabaseResults <> buildsSdkResults
        setUpDatabaseResults -> return $ npmInstallResults <> setUpDatabaseResults
    npmInstallResults -> return npmInstallResults

runNpmInstallIfNeeded :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
runNpmInstallIfNeeded spec dstDir sendMessage = do
  isNpmInstallNeeded spec dstDir >>= \case
    Left errorMessage -> return ([], [GenericGeneratorError errorMessage])
    Right maybeFullStackDeps -> case maybeFullStackDeps of
      Nothing -> return ([], [])
      Just fullStackDeps -> do
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

buildSdk :: Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
buildSdk projectRootDir sendMessage = do
  sendMessage $ Msg.Start "Building SDK..."
  SdkGenerator.buildSdk projectRootDir >>= \case
    Left errorMesage -> return ([], [GenericGeneratorError errorMesage])
    Right () -> do
      sendMessage $ Msg.Success "SDK built successfully."
      return ([], [])
