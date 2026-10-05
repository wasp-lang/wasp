module Wasp.Generator.Setup
  ( runSetup,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Either (fromLeft)
import StrongPath (Abs, Dir, Path')
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.DbGenerator as DbGenerator
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import Wasp.Generator.NpmInstall (installNpmDependenciesWithInstallRecord)
import qualified Wasp.Generator.SdkGenerator as SdkGenerator
import Wasp.Generator.WebAppGenerator (createWebAppRootDir)
import qualified Wasp.Message as Msg

type Setup = ExceptT [GeneratorError] (WriterT [GeneratorWarning] IO)

runSetup :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
runSetup spec generatedAppDir sendMessage = do
  (result, warnings) <- runWriterT $ runExceptT $ do
    installDependencies spec generatedAppDir sendMessage
    setUpDatabase spec generatedAppDir sendMessage
    -- todo(filip): Should we consider building SDK as part of code generation?
    -- todo(filip): Avoid building on each setup if we don't need to.
    buildSdk generatedAppDir sendMessage
    liftIO $ createWebAppRootDir generatedAppDir
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
