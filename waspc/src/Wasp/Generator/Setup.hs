module Wasp.Generator.Setup
  ( runSetup,
  )
where

import Control.Monad (when)
import StrongPath (Abs, Dir, Path')
import System.IO (hFlush, hPutStrLn, stderr)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.DbGenerator as DbGenerator
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import Wasp.Generator.NpmInstall (installNpmDependenciesWithInstallRecord)
import qualified Wasp.Generator.SdkGenerator as SdkGenerator
import Wasp.Generator.WebAppGenerator (createWebAppRootDir)
import qualified Wasp.Message as Msg

debugLog :: String -> IO ()
debugLog msg = hPutStrLn stderr ("[DEBUG Setup] " ++ msg) >> hFlush stderr

runSetup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
runSetup spec projectRootDir sendMessage = do
  debugLog "Starting runSetup"
  debugLog "Starting npm install..."
  installNpmDependenciesWithInstallRecord spec projectRootDir >>= \case
    Right () -> do
      debugLog "npm install completed successfully"
      sendMessage $ Msg.Success "Successfully completed npm install."
      debugLog "Starting database setup..."
      setUpDatabase spec projectRootDir sendMessage >>= \case
        setUpDatabaseResults@(_warnings, _errors@[]) -> do
          debugLog "Database setup completed successfully"
          -- todo(filip): Should we consider building SDK as part of code generation?
          -- todo(filip): Avoid building on each setup if we don't need to.
          debugLog "Starting SDK build..."
          buildSdkResults <- buildSdk projectRootDir sendMessage
          debugLog "SDK build completed"
          debugLog "Creating web app root dir..."
          createWebAppRootDir projectRootDir
          debugLog "Web app root dir created. runSetup finished."
          return $ setUpDatabaseResults <> buildSdkResults
        setUpDatabaseResults -> do
          debugLog "Database setup had errors, skipping SDK build"
          return setUpDatabaseResults
    Left npmInstallError -> do
      debugLog $ "npm install failed: " ++ show npmInstallError
      return ([], [npmInstallError])

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
