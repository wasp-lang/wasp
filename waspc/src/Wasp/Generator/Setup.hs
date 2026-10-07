module Wasp.Generator.Setup
  ( SetupStep (..),
    allSetupSteps,
    runSetup,
    orderSetupSteps, -- Exported for testing.
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Either (fromLeft)
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
import Wasp.Generator.WebAppGenerator (createWebAppRootDir)
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Message as Msg

-- | The work that turns freshly generated code into a runnable app.
--
-- The constructors are declared in the order the steps must run: npm install
-- provides the tooling for everything after it, and the Prisma client has to
-- exist before the SDK that imports it is built.
data SetupStep
  = InstallNpmDeps
  | FormatPrismaSchema
  | WarnIfDbNeedsMigration
  | GeneratePrismaClient
  | BuildSdk
  | CreateWebAppRootDir
  | TypeCheckUserCode
  deriving (Eq, Show, Enum, Bounded)

allSetupSteps :: [SetupStep]
allSetupSteps = [minBound .. maxBound]

type Setup = ExceptT [GeneratorError] (WriterT [GeneratorWarning] IO)

-- | Runs the requested steps in their declaration order, whatever order they
-- were requested in, and stops at the first step that fails.
runSetup :: [SetupStep] -> AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
runSetup requestedSteps spec generatedAppDir sendMessage = do
  (result, warnings) <-
    runWriterT $
      runExceptT $
        forM_ (orderSetupSteps requestedSteps) $ \step ->
          runSetupStep step spec generatedAppDir sendMessage
  return (warnings, fromLeft [] result)

orderSetupSteps :: [SetupStep] -> [SetupStep]
orderSetupSteps requestedSteps = filter (`elem` requestedSteps) allSetupSteps

runSetupStep :: SetupStep -> AppSpec -> Path' Abs (Dir GeneratedAppDir) -> Msg.SendMessage -> Setup ()
runSetupStep step spec generatedAppDir sendMessage = case step of
  InstallNpmDeps -> installDependencies spec generatedAppDir sendMessage
  FormatPrismaSchema -> liftIO $ DbGenerator.formatPrismaSchemaFileOnDisk generatedAppDir
  WarnIfDbNeedsMigration -> warnIfDbNeedsMigration spec generatedAppDir
  GeneratePrismaClient -> generatePrismaClient spec generatedAppDir sendMessage
  -- todo(filip): Should we consider building SDK as part of code generation?
  -- todo(filip): Avoid building on each setup if we don't need to.
  BuildSdk -> buildSdk generatedAppDir sendMessage
  CreateWebAppRootDir -> liftIO $ createWebAppRootDir generatedAppDir
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
