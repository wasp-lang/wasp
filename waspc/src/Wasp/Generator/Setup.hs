module Wasp.Generator.Setup
  ( SetupStep (..),
    allSetupSteps,
    setUpGeneratedApp,
    runSetup,
    deduplicateAndOrderSetupSteps, -- Exported for testing.
  )
where

import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Either (fromLeft)
import Data.Maybe (maybeToList)
import StrongPath (Abs, Dir, Path')
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.DbGenerator as DbGenerator
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import Wasp.Generator.NpmInstall (installNpmDependenciesWithInstallRecord)
import qualified Wasp.Generator.SdkGenerator as SdkGenerator
import Wasp.Generator.WebAppGenerator (createWebAppRootDir)
import qualified Wasp.Message as Msg

-- | Setup steps that a command might need post code generation.
--
-- The constructors are declared in the order the steps must execute.
-- I.e. the 'InstallNpmDeps' will always run first.
data SetupStep
  = InstallNpmDeps
  | FormatPrismaSchema
  | WarnIfDbNeedsMigration
  | GeneratePrismaClient
  | BuildSdk
  | CreateWebAppRootDir
  deriving (Eq, Show, Enum, Bounded)

allSetupSteps :: [SetupStep]
allSetupSteps = [minBound .. maxBound]

type Setup = ExceptT [GeneratorError] (WriterT [GeneratorWarning] IO)

-- | Runs the requested setup steps in a pre-determined order.
-- Stops at the first step that fails.
setUpGeneratedApp :: [SetupStep] -> AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
setUpGeneratedApp requestedSteps spec generatedAppDir sendMessage =
  forM_ (deduplicateAndOrderSetupSteps requestedSteps) $ \step ->
    runSetupStep step spec generatedAppDir sendMessage

runSetup :: Setup a -> IO ([GeneratorWarning], [GeneratorError])
runSetup setupAction = do
  (result, warnings) <- runWriterT $ runExceptT setupAction
  return (warnings, fromLeft [] result)

-- | Deduplicates and orders the steps.
-- The steps must execute in the order of 'SetupStep' constructor declarations.
deduplicateAndOrderSetupSteps :: [SetupStep] -> [SetupStep]
deduplicateAndOrderSetupSteps requestedSteps = filter (`elem` requestedSteps) allSetupSteps

runSetupStep :: SetupStep -> AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
runSetupStep step spec generatedAppDir sendMessage = case step of
  InstallNpmDeps -> installDependencies spec generatedAppDir sendMessage
  FormatPrismaSchema -> liftIO $ DbGenerator.formatPrismaSchemaFileOnDisk generatedAppDir
  -- A production build is deployed elsewhere, so there is no local database
  -- to check for pending migrations.
  WarnIfDbNeedsMigration -> when (AS.isDevelopment spec) $ warnIfDbNeedsMigration spec generatedAppDir
  GeneratePrismaClient -> generatePrismaClient spec generatedAppDir sendMessage
  -- todo(filip): Should we consider building SDK as part of code generation?
  -- todo(filip): Avoid building on each setup if we don't need to.
  BuildSdk -> buildSdk generatedAppDir sendMessage
  CreateWebAppRootDir -> liftIO $ createWebAppRootDir generatedAppDir

installDependencies :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
installDependencies spec generatedAppDir sendMessage = do
  result <- liftIO $ installNpmDependenciesWithInstallRecord spec generatedAppDir
  case result of
    Left npmInstallError -> throwError [npmInstallError]
    Right () -> liftIO $ sendMessage $ Msg.Success "Successfully completed npm install."

warnIfDbNeedsMigration :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Setup ()
warnIfDbNeedsMigration spec generatedAppDir = do
  warning <- liftIO $ DbGenerator.warnIfDbNeedsMigration spec generatedAppDir
  tell $ maybeToList warning

generatePrismaClient :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
generatePrismaClient spec generatedAppDir sendMessage = do
  result <- liftIO $ do
    sendMessage $ Msg.Start "Generating Prisma client..."
    DbGenerator.generatePrismaClient spec generatedAppDir
  case result of
    Just generatorError -> throwError [generatorError]
    Nothing -> liftIO $ sendMessage $ Msg.Success "Prisma client is up to date."

buildSdk :: Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
buildSdk generatedAppDir sendMessage = do
  result <- liftIO $ do
    sendMessage $ Msg.Start "Building SDK..."
    SdkGenerator.buildSdk generatedAppDir
  case result of
    Left errorMessage -> throwError [GenericGeneratorError errorMessage]
    Right () -> liftIO $ sendMessage $ Msg.Success "SDK built successfully."
