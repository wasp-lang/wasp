module Wasp.Generator.Setup
  ( runSetup,
  )
where

import Control.Monad (when)
import StrongPath (Abs, Dir, Path')
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.DbGenerator as DbGenerator
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import Wasp.Generator.NpmInstall (installNpmDependenciesWithInstallRecord)
import qualified Wasp.Generator.SdkGenerator as SdkGenerator
import qualified Wasp.Message as Msg

runSetup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
runSetup spec projectRootDir sendMessage = do
  installNpmDependenciesWithInstallRecord spec projectRootDir >>= \case
    Right () -> do
      sendMessage $ Msg.Success "Successfully completed npm install."
      setUpDatabase spec projectRootDir sendMessage >>= \case
        setUpDatabaseResults@(_warnings, _errors@[]) -> do
          -- todo(filip): Should we consider building SDK as part of code generation?
          -- todo(filip): Avoid building on each setup if we don't need to.
          buildsSdkResults <- buildSdk projectRootDir sendMessage
          return $ setUpDatabaseResults <> buildsSdkResults
        setUpDatabaseResults -> return setUpDatabaseResults
    Left npmInstallError -> return ([], [npmInstallError])

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
