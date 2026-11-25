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

import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Either (fromLeft)
import Data.List (nub)
import Data.Maybe (maybeToList)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.DbGenerator as DbGenerator
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import qualified Wasp.Generator.NodeCommand as NodeCommand
import qualified Wasp.Generator.NodeCommands as NodeCommands
import Wasp.Generator.NpmInstall (installNpmDependenciesWithInstallRecord)
import qualified Wasp.Generator.SdkGenerator as SdkGenerator
import qualified Wasp.Job as J
import Wasp.Project.Db.Dev (devDatabaseEnvVars, findDevDatabaseUrl)
import Wasp.TaskRunner (TaskRunner (..))
import Wasp.Util.IO.Process (WriteOutput)

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
prerequisites FormatPrismaSchema = [InstallNpmDeps]
prerequisites WarnIfDbNeedsMigration = [InstallNpmDeps, FormatPrismaSchema]
prerequisites GeneratePrismaClient = [InstallNpmDeps, FormatPrismaSchema]
-- Runs `tsc`, and the SDK imports the Prisma client.
prerequisites BuildSdk = [InstallNpmDeps, GeneratePrismaClient]
-- Runs `tsc`, and the user code imports the SDK.
prerequisites TypeCheckUserCode = [InstallNpmDeps, BuildSdk]

type Setup = ExceptT [GeneratorError] (WriterT [GeneratorWarning] IO)

-- | Runs the setup steps the goals need, each after its prerequisites.
-- Stops at the first step that fails.
setUpGeneratedApp :: [SetupGoal] -> AppSpec -> Path' Abs (Dir GeneratedAppDir) -> TaskRunner -> WriteOutput -> Setup ()
setUpGeneratedApp goals spec generatedAppDir taskRunner writeOutput =
  forM_ (resolveSetupSteps goals) $ \step ->
    runSetupStep step spec generatedAppDir taskRunner writeOutput

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

runSetupStep :: SetupStep -> AppSpec -> Path' Abs (Dir GeneratedAppDir) -> TaskRunner -> WriteOutput -> Setup ()
runSetupStep step spec generatedAppDir taskRunner writeOutput = case step of
  InstallNpmDeps -> installDependencies spec generatedAppDir taskRunner writeOutput
  FormatPrismaSchema -> formatPrismaSchema generatedAppDir
  -- A production build is deployed elsewhere, so there is no local database
  -- to check for pending migrations.
  WarnIfDbNeedsMigration -> when (AS.isDevelopment spec) $ warnIfDbNeedsMigration spec generatedAppDir writeOutput
  GeneratePrismaClient -> generatePrismaClient spec generatedAppDir taskRunner writeOutput
  -- todo(filip): Should we consider building SDK as part of code generation?
  -- todo(filip): Avoid building on each setup if we don't need to.
  BuildSdk -> buildSdk generatedAppDir taskRunner writeOutput
  TypeCheckUserCode -> typeCheckUserCode spec taskRunner writeOutput

installDependencies :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> TaskRunner -> WriteOutput -> Setup ()
installDependencies spec generatedAppDir taskRunner writeOutput =
  runTask taskRunner "Installing dependencies" "Installed dependencies" "Failed to install dependencies" $ do
    result <- liftIO $ installNpmDependenciesWithInstallRecord writeOutput spec generatedAppDir
    either (throwError . pure) return result

warnIfDbNeedsMigration :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> WriteOutput -> Setup ()
warnIfDbNeedsMigration spec generatedAppDir writeOutput = do
  warning <- liftIO $ do
    envVars <- devDatabaseEnvVars <$> findDevDatabaseUrl spec
    DbGenerator.warnIfDbNeedsMigration writeOutput envVars spec generatedAppDir
  tell $ maybeToList warning

formatPrismaSchema :: Path' Abs (Dir GeneratedAppDir) -> Setup ()
formatPrismaSchema generatedAppDir = do
  result <- liftIO $ DbGenerator.formatPrismaSchema generatedAppDir
  case result of
    Just generatorError -> throwError [generatorError]
    Nothing -> return ()

generatePrismaClient :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> TaskRunner -> WriteOutput -> Setup ()
generatePrismaClient spec generatedAppDir taskRunner writeOutput =
  runTask taskRunner "Generating the Prisma client" "The Prisma client is up to date" "Failed to generate the Prisma client" $ do
    maybeGeneratorError <- liftIO $ DbGenerator.generatePrismaClient writeOutput spec generatedAppDir
    forM_ maybeGeneratorError $ throwError . pure

buildSdk :: Path' Abs (Dir GeneratedAppDir) -> TaskRunner -> WriteOutput -> Setup ()
buildSdk generatedAppDir taskRunner writeOutput =
  runTask taskRunner "Building the SDK" "Built the SDK" "Failed to build the SDK" $ do
    result <- liftIO $ SdkGenerator.buildSdk writeOutput generatedAppDir
    either (throwError . pure . GenericGeneratorError) return result

typeCheckUserCode :: AppSpec -> TaskRunner -> WriteOutput -> Setup ()
typeCheckUserCode spec taskRunner writeOutput =
  runTask taskRunner "Type-checking user code" "Type-checked user code" "Failed to type-check user code" $ do
    exitCode <- liftIO $ J.run writeOutput $ J.prefixWith J.Wasp typeCheck
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure code ->
        throwError [GenericGeneratorError $ "User code type-check failed with exit code: " ++ show code]
  where
    typeCheck :: J.Job ExitCode
    typeCheck =
      J.fromProc $
        NodeCommand.proc
          (AS.waspProjectDir spec)
          (NodeCommands.typeCheckUserCode $ AS.srcTsConfigPath spec)
