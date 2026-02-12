module Wasp.Cli.Command.Build
  ( build,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.Chan (newChan)
import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', fromRelDir, (</>))
import qualified StrongPath as SP
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Build.DockerBuildContext (prepareFilesNecessaryForDockerBuild)
import Wasp.Cli.Command.Compile (analyze, compileIOWithOptions, printCompilationResult)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Message (cliSendMessage)
import Wasp.CompileOptions (CompileOptions (..))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))
import qualified Wasp.Generator.SdkGenerator.Client.VitePlugin.Common as ViteCommon
import Wasp.Generator.WebAppGenerator
  ( hasSsrEnabledPage,
    viteSsrBuildDirInWebAppDir,
    webAppRootDirInProjectRootDir,
  )
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Message as Msg
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Common
  ( CompileError,
    CompileWarning,
    WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir
  )
import Wasp.Util.IO (doesDirectoryExist, removeDirectory)

-- | Builds Wasp project that the current working directory is part of.
-- Does all the steps, from analysis to generation, and at the end writes generated code
-- to the disk, to the .wasp/out dir.
-- At the end, prints a report on how building went (by printing warnings, errors,
-- success/failure message, further steps, ...).
-- Finally, throws if there was a compile/build error.
-- Very similar to 'compile'.
build :: Command ()
build = do
  InWaspProject waspProjectDir <- require

  let buildDirInWaspProjectDir = dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir
      buildDir = waspProjectDir </> buildDirInWaspProjectDir

  doesBuildDirExist <- liftIO $ doesDirectoryExist buildDir
  when doesBuildDirExist $ do
    cliSendMessageC $
      Msg.Start $
        "Clearing the content of the " ++ fromRelDir buildDirInWaspProjectDir ++ " directory..."
    liftIO $ removeDirectory buildDir
    cliSendMessageC $
      Msg.Success $
        "Successfully cleared the contents of the " ++ fromRelDir buildDirInWaspProjectDir ++ " directory."

  cliSendMessageC $ Msg.Start "Building wasp project..."

  (warnings, errors) <- liftIO $ buildIO waspProjectDir buildDir
  liftIO $ printCompilationResult (warnings, errors)
  unless (null errors) $
    throwError $
      CommandError "Building of wasp project failed" $
        show (length errors) ++ " errors found."

  liftIO (prepareFilesNecessaryForDockerBuild waspProjectDir buildDir) >>= \case
    Left err -> throwError $ CommandError "Failed to prepare files necessary for docker build" err
    Right () -> return ()

  -- Build the Vite client and SSR bundles so that .wasp/out/web-app/ is
  -- deployment-ready (build/ and build-ssr/ directories).
  appSpec <- analyze waspProjectDir
  let ssrEnabled = hasSsrEnabledPage appSpec

  cliSendMessageC $ Msg.Start "Building client..."
  runAndPrintJob "Building the client failed." $
    buildViteClient waspProjectDir
  cliSendMessageC $ Msg.Success "Client built."

  when ssrEnabled $ do
    cliSendMessageC $ Msg.Start "Building SSR bundle..."
    runAndPrintJob "Building the SSR bundle failed." $
      buildViteSsr waspProjectDir
    cliSendMessageC $ Msg.Success "SSR bundle built."

  cliSendMessageC $
    Msg.Success $
      "Your wasp project has been successfully built! Check it out in the " ++ fromRelDir buildDirInWaspProjectDir ++ " directory."

buildIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  IO ([CompileWarning], [CompileError])
buildIO waspProjectDir buildDir = compileIOWithOptions options waspProjectDir buildDir
  where
    options =
      CompileOptions
        { waspProjectDirPath = waspProjectDir,
          buildType = BuildType.Production,
          sendMessage = cliSendMessage,
          -- Ignore "DB needs migration warnings" during build, as that is not a required step.
          generatorWarningsFilter =
            filter
              ( \case
                  GeneratorNeedsMigrationWarning _ -> False
                  _ -> True
              )
        }

-- | Builds the Vite client bundle (produces .wasp/out/web-app/build/).
buildViteClient :: Path' Abs (Dir WaspProjectDir) -> ExceptJob
buildViteClient waspProjectDir =
  runNodeCommandAsJob waspProjectDir "npx" ["vite", "build"] J.WebApp
    & toExceptJob (("Building the client failed with exit code: " <>) . show)

-- | Builds the Vite SSR bundle (produces .wasp/out/web-app/build-ssr/).
buildViteSsr :: Path' Abs (Dir WaspProjectDir) -> ExceptJob
buildViteSsr waspProjectDir =
  runNodeCommandAsJob waspProjectDir "npx" ["vite", "build", "--ssr", ViteCommon.ssrEntryPointPath, "--outDir", ssrBuildOutDir] J.WebApp
    & toExceptJob (("Building the SSR bundle failed with exit code: " <>) . show)
  where
    ssrBuildOutDir =
      SP.fromRelDir
        ( dotWaspDirInWaspProjectDir
            </> generatedCodeDirInDotWaspDir
            </> webAppRootDirInProjectRootDir
            </> viteSsrBuildDirInWebAppDir
        )

-- | Runs an ExceptJob, printing its output messages, and throws a CommandError on failure.
runAndPrintJob :: String -> ExceptJob -> Command ()
runAndPrintJob errorMessage job = do
  liftIO (runAndPrintJobIO job)
    >>= either (throwError . CommandError errorMessage) return

runAndPrintJobIO :: ExceptJob -> IO (Either String ())
runAndPrintJobIO job = do
  chan <- newChan
  (result, _) <-
    concurrently
      (runExceptT $ job chan)
      (readJobMessagesAndPrintThemPrefixed chan)
  return result
