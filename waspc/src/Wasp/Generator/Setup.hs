module Wasp.Generator.Setup
  ( setUpGeneratedApp,
    runSetup,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Either (fromLeft)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.DbGenerator as DbGenerator
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import Wasp.Generator.NpmInstall (installNpmDependenciesWithInstallRecord)
import qualified Wasp.Generator.SdkGenerator as SdkGenerator
import Wasp.Generator.WebAppGenerator (createWebAppRootDir)
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Message as Msg

type Setup = ExceptT [GeneratorError] (WriterT [GeneratorWarning] IO)

setUpGeneratedApp :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
setUpGeneratedApp spec generatedAppDir sendMessage = do
  installDependencies spec generatedAppDir sendMessage
  setUpDatabase spec generatedAppDir sendMessage
  -- todo(filip): Should we consider building SDK as part of code generation?
  -- todo(filip): Avoid building on each setup if we don't need to.
  buildSdk generatedAppDir sendMessage
  liftIO $ createWebAppRootDir generatedAppDir
  typeCheckUserCode spec sendMessage

runSetup :: Setup a -> IO ([GeneratorWarning], [GeneratorError])
runSetup setupAction = do
  (result, warnings) <- runWriterT $ runExceptT setupAction
  return (warnings, fromLeft [] result)

installDependencies :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
installDependencies spec generatedAppDir sendMessage = do
  result <- liftIO $ installNpmDependenciesWithInstallRecord spec generatedAppDir
  case result of
    Left npmInstallError -> throwError [npmInstallError]
    Right () -> liftIO $ sendMessage $ Msg.Success "Successfully completed npm install."

setUpDatabase :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
setUpDatabase spec dstDir sendMessage = do
  (dbGeneratorWarnings, dbGeneratorErrors) <- liftIO $ do
    sendMessage $ Msg.Start "Setting up database..."
    DbGenerator.postWriteDbGeneratorActions spec dstDir
  tell dbGeneratorWarnings
  unless (null dbGeneratorErrors) $ throwError dbGeneratorErrors
  liftIO $ sendMessage $ Msg.Success "Database successfully set up."

buildSdk :: Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
buildSdk generatedAppDir sendMessage = do
  result <- liftIO $ do
    sendMessage $ Msg.Start "Building SDK..."
    SdkGenerator.buildSdk generatedAppDir
  case result of
    Left errorMessage -> throwError [GenericGeneratorError errorMessage]
    Right () -> liftIO $ sendMessage $ Msg.Success "SDK built successfully."

typeCheckUserCode :: AppSpec -> Msg.SendMessage -> Setup ()
typeCheckUserCode spec sendMessage = do
  (_, exitCode) <- liftIO $ do
    sendMessage $ Msg.Start "Type-checking user code..."
    chan <- newChan
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (runTypeCheck chan)
  case exitCode of
    ExitSuccess -> liftIO $ sendMessage $ Msg.Success "User code type-checked successfully."
    ExitFailure code ->
      throwError [GenericGeneratorError $ "User code type-check failed with exit code: " ++ show code]
  where
    runTypeCheck :: J.Job
    runTypeCheck =
      runNodeCommandAsJob
        (AS.waspProjectDir spec)
        "npx"
        [ "tsc",
          "--project " ++ SP.fromRelFile (AS.srcTsConfigPath spec),
          "--noEmit"
        ]
        J.Wasp
