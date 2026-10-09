module Wasp.Generator.Setup
  ( SetupGoal (..),
    setUpGeneratedApp,
    runSetup,
    SetupStep (..), -- Exported for testing.
    allSetupSteps, -- Exported for testing.
    setupStepsFor, -- Exported for testing.
    prerequisites, -- Exported for testing.
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

-- | How far to set up the generated app after code generation.
--
-- Each goal includes everything the goals before it prepare.
data SetupGoal
  = -- | The Prisma CLI is installed and can read the schema.
    PrismaCliReady
  | -- | The SDK is built, along with the Prisma client it imports.
    SdkReady
  | -- | The generated app has everything it needs to run or be built.
    GeneratedAppReady
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Setup steps that a goal might need post code generation.
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

-- | The steps that must run before the step can run.
-- A step must be declared after all of its prerequisites.
--
-- 'InstallNpmDeps' installs the node modules of the whole project (user code,
-- SDK, server, web app) in one go through npm workspaces, so every step that
-- runs a Node tool depends on it.
-- 'FormatPrismaSchema' matters for the steps that compare schema checksums:
-- Prisma formats the schema it keeps, so ours has to be formatted the same way
-- for the checksums to match.
prerequisites :: SetupStep -> [SetupStep]
prerequisites InstallNpmDeps = []
prerequisites FormatPrismaSchema = []
prerequisites CreateWebAppRootDir = []
-- Compares the schema to the checksum from the last migration, or asks the
-- Prisma CLI to compare it to the database.
prerequisites WarnIfDbNeedsMigration = [InstallNpmDeps, FormatPrismaSchema]
-- Runs the Prisma CLI, and skips regenerating when the schema checksum still
-- matches the one from the last generation.
prerequisites GeneratePrismaClient = [InstallNpmDeps, FormatPrismaSchema]
-- Runs tsc, and the SDK imports the Prisma client.
prerequisites BuildSdk = [InstallNpmDeps, GeneratePrismaClient]

type Setup = ExceptT [GeneratorError] (WriterT [GeneratorWarning] IO)

-- | Runs the setup steps the goal needs, in a pre-determined order.
-- Stops at the first step that fails.
setUpGeneratedApp :: SetupGoal -> AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
setUpGeneratedApp goal spec generatedAppDir sendMessage =
  forM_ (setupStepsFor goal) $ \step ->
    runSetupStep step spec generatedAppDir sendMessage

runSetup :: Setup a -> IO ([GeneratorWarning], [GeneratorError])
runSetup setupAction = do
  (result, warnings) <- runWriterT $ runExceptT setupAction
  return (warnings, fromLeft [] result)

-- | The steps the goal needs, with their prerequisites, in the order of
-- 'SetupStep' constructor declarations.
setupStepsFor :: SetupGoal -> [SetupStep]
setupStepsFor = deduplicateAndOrderSetupSteps . withPrerequisites . targetSteps

-- | The steps a goal is after. 'setupStepsFor' adds their prerequisites.
targetSteps :: SetupGoal -> [SetupStep]
targetSteps PrismaCliReady = [InstallNpmDeps, FormatPrismaSchema]
targetSteps SdkReady = [BuildSdk]
targetSteps GeneratedAppReady = allSetupSteps

withPrerequisites :: [SetupStep] -> [SetupStep]
withPrerequisites steps = steps ++ concatMap (withPrerequisites . prerequisites) steps

-- | Deduplicates and orders the steps.
-- The steps must execute in the order of 'SetupStep' constructor declarations.
deduplicateAndOrderSetupSteps :: [SetupStep] -> [SetupStep]
deduplicateAndOrderSetupSteps steps = filter (`elem` steps) allSetupSteps

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
