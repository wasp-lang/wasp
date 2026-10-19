module Wasp.Generator.Setup
  ( SetupGoal (..),
    allSetupGoals,
    setUpGeneratedApp,
    runSetup,
    SetupStep (..), -- Exported for testing.
    allSetupSteps, -- Exported for testing.
    resolveSetupSteps, -- Exported for testing.
    prerequisites, -- Exported for testing.
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Either (fromLeft)
import Data.List (nub)
import Data.Maybe (maybeToList)
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
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Message as Msg

-- | Something a command needs from the generated app, after code generation.
data SetupGoal
  = PrismaCliReady
  | DbMigrationsChecked
  | SdkReady
  | UserCodeTypeChecked
  deriving (Eq, Show, Enum, Bounded)

-- | What running or building the generated app needs.
allSetupGoals :: [SetupGoal]
allSetupGoals = [minBound .. maxBound]

-- | Setup steps that a goal might need post code generation.
data SetupStep
  = InstallNpmDeps
  | FormatPrismaSchema
  | WarnIfDbNeedsMigration
  | GeneratePrismaClient
  | BuildSdk
  | TypeCheckUserCode
  deriving (Eq, Show, Enum, Bounded)

allSetupSteps :: [SetupStep]
allSetupSteps = [minBound .. maxBound]

-- | The prerequisite steps that must run before the provided step.
--
-- 'InstallNpmDeps' installs the node modules of the whole project (user project,
-- SDK, generated server), so the steps that use installed packages depend on it.
-- 'FormatPrismaSchema' matters for the steps that compare schema checksums.
prerequisites :: SetupStep -> [SetupStep]
prerequisites InstallNpmDeps = []
prerequisites FormatPrismaSchema = []
prerequisites WarnIfDbNeedsMigration = [InstallNpmDeps, FormatPrismaSchema]
prerequisites GeneratePrismaClient = [InstallNpmDeps, FormatPrismaSchema]
-- Runs `tsc`, and the SDK imports the Prisma client.
prerequisites BuildSdk = [InstallNpmDeps, GeneratePrismaClient]
-- Runs `tsc`, and the user code imports the SDK.
prerequisites TypeCheckUserCode = [InstallNpmDeps, BuildSdk]

type Setup = ExceptT [GeneratorError] (WriterT [GeneratorWarning] IO)

-- | Runs the setup steps the goals need, each after its prerequisites.
-- Stops at the first step that fails.
setUpGeneratedApp :: [SetupGoal] -> AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
setUpGeneratedApp goals spec generatedAppDir sendMessage =
  forM_ (resolveSetupSteps goals) $ \step ->
    runSetupStep step spec generatedAppDir sendMessage

runSetup :: Setup a -> IO ([GeneratorWarning], [GeneratorError])
runSetup setupAction = do
  (result, warnings) <- runWriterT $ runExceptT setupAction
  return (warnings, fromLeft [] result)

-- | The steps the goals need, in correct execution order.
resolveSetupSteps :: [SetupGoal] -> [SetupStep]
resolveSetupSteps = withPrerequisites . concatMap stepsForGoal

stepsForGoal :: SetupGoal -> [SetupStep]
stepsForGoal PrismaCliReady = [InstallNpmDeps, FormatPrismaSchema]
stepsForGoal DbMigrationsChecked = [WarnIfDbNeedsMigration]
stepsForGoal SdkReady = [BuildSdk]
stepsForGoal UserCodeTypeChecked = [TypeCheckUserCode]

-- | The steps with all their prerequisites. Each step comes after its
-- prerequisites, and appears once.
withPrerequisites :: [SetupStep] -> [SetupStep]
withPrerequisites = nub . concatMap prerequisitesThenStep
  where
    prerequisitesThenStep step = concatMap prerequisitesThenStep (prerequisites step) ++ [step]

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
  TypeCheckUserCode -> typeCheckUserCode spec sendMessage

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
          "--project",
          SP.fromRelFile $ AS.srcTsConfigPath spec,
          "--noEmit"
        ]
        J.Wasp
