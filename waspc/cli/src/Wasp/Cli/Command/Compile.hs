{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Cli.Command.Compile
  ( compileIO,
    compileCommand,
    compile,
    compileWithOptions,
    ensureCompile,
    waitEnsureCompile,
    compileIOWithOptions,
    defaultCompileOptions,
    printCompilationResult,
    printWarningsAndErrorsIfAny,
    analyze,
    analyzeWithOptions,
    analyzeWithDiagnosticsOnStderr,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromLeft)
import Data.List (intercalate)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.Info
import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Cli.ProjectLock (tryWithProjectLock, withProjectLock)
import Wasp.Cli.ProjectLock.Data (ProjectLockData (..), WaspProcessId, WatcherStatus (..))
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Generator
import qualified Wasp.Generator.WaspInfo as WaspInfo
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning, WaspProjectDir)
import qualified Wasp.Project
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)
import Wasp.Util.IO (doesDirectoryExist, removeDirectory)

-- | Meant for the standalone `wasp compile` command: commands that hold the
-- project lock themselves should call 'compile' instead.
compileCommand :: Command ([CompileWarning], AS.AppSpec)
compileCommand = withProjectLock compile

-- | Same like 'compileWithOptions', but with default compile options.
compile :: Command ([CompileWarning], AS.AppSpec)
compile = do
  -- TODO: Consider a way to remove the redundancy of finding the project root
  -- here and in compileWithOptions. One option could be to add this to defaultCompileOptions
  -- add make externalCodeDirPath a helper function, along with any others we typically need.
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  compileWithOptions $ defaultCompileOptions waspProjectDir

-- | Compiles Wasp project that the current working directory is part of.
-- Does all the steps, from analysis to generation, and at the end writes generated code
-- to the disk, to the .wasp dir.
-- At the end, prints a report on how compilation went (by printing warnings, errors,
-- success/failure message, ...).
-- Finally, throws if there was a compile error, otherwise returns any compile warnings
-- along with the AppSpec it compiled.
compileWithOptions :: CompileOptions -> Command ([CompileWarning], AS.AppSpec)
compileWithOptions options = do
  ValidNodeAndNpm <- require
  InWaspProject waspProjectDir <- require

  let outDir = waspProjectDir </> generatedAppDirInWaspProjectDir

  generatedAppIsCompatible <-
    liftIO $ buildType options `WaspInfo.isCompatibleWithExistingBuildAt` outDir

  outDirExists <- liftIO $ doesDirectoryExist outDir

  when (outDirExists && not generatedAppIsCompatible) $ do
    cliSendMessageC $
      Msg.Start $
        "Clearing the content of the " ++ SP.fromRelDir generatedAppDirInWaspProjectDir ++ " directory..."
    liftIO $ removeDirectory outDir
    cliSendMessageC $
      Msg.Success $
        "Successfully cleared the contents of the " ++ SP.fromRelDir generatedAppDirInWaspProjectDir ++ " directory."

  cliSendMessageC $ Msg.Start "Compiling wasp project..."
  (warnings, appSpecOrErrors) <- liftIO $ compileIOWithOptions options waspProjectDir outDir

  liftIO $ printCompilationResult (warnings, fromLeft [] appSpecOrErrors)
  case appSpecOrErrors of
    Right appSpec -> return (warnings, appSpec)
    Left errors ->
      throwError $
        CommandError "Compilation of wasp project failed" $
          show (length errors) ++ " errors found"

-- | Makes sure the generated app is up to date with the project's source, for
-- commands that only need to use it. It doesn't hold the project lock
-- afterwards, so the command can run next to other Wasp commands:
--   - If no other Wasp command is working on the project, it compiles the
--     project while holding the lock.
--   - If a watcher (e.g. `wasp start`) is keeping the generated app up to date,
--     it trusts it and skips compiling.
--   - If that watcher failed to compile the project, it throws.
--   - If any other Wasp command is working on the project, it throws too. See
--     'waitEnsureCompile' for a version that waits for it to finish instead.
ensureCompile :: CompileOptions -> Command ()
ensureCompile options =
  tryEnsureCompile options >>= \case
    GeneratedAppUpToDate -> return ()
    ProjectBusy maybeProcessId -> throwError $ makeProjectBusyError maybeProcessId

-- | Like 'ensureCompile', but if another Wasp command is working on the
-- project, it waits a bit for it to finish before giving up.
--
-- NOTE: On Windows, it doesn't wait. There, the lock stops other processes
-- from reading the lock file, so we can never tell what the lock holder is
-- doing, and waiting would only delay the same error.
waitEnsureCompile :: CompileOptions -> Command ()
waitEnsureCompile options
  | System.Info.os == "mingw32" = ensureCompile options
  | otherwise = attempt maxRetries
  where
    attempt retriesLeft =
      tryEnsureCompile options >>= \case
        GeneratedAppUpToDate -> return ()
        ProjectBusy maybeProcessId
          | retriesLeft <= 0 -> throwError $ makeProjectBusyError maybeProcessId
          | otherwise -> do
              when (retriesLeft == maxRetries) $
                cliSendMessageC $
                  Msg.Start "Waiting for compilation to finish..."
              liftIO $ threadDelay retryDelayInMicroseconds
              attempt $ retriesLeft - 1

    maxRetries = 10 :: Int
    retryDelayInMicroseconds = 1000000

data EnsureCompileResult
  = -- | The generated app is up to date with the project's source.
    GeneratedAppUpToDate
  | -- | Another Wasp command is working on the project, so the generated app
    -- may change under us. Trying again once it finishes may work.
    ProjectBusy (Maybe WaspProcessId)

tryEnsureCompile :: CompileOptions -> Command EnsureCompileResult
tryEnsureCompile options =
  tryWithProjectLock (compileWithOptions options) >>= \case
    Right _ -> return GeneratedAppUpToDate
    Left (Just ProjectLockData {pid, watcherStatus = Just UpToDate}) -> do
      cliSendMessageC $
        Msg.Info $
          "Skipping compilation, another Wasp command (PID "
            ++ show pid
            ++ ") is already keeping this project compiled."
      return GeneratedAppUpToDate
    Left (Just ProjectLockData {pid, watcherStatus = Just CompilationFailed}) ->
      throwError $
        CommandError "Wasp project failed to compile" $
          "Another Wasp command (PID "
            ++ show pid
            ++ ") is running for this project, but it failed to compile it."
            ++ " Fix the compilation errors it reported, and run this command again once it compiles the project successfully."
    Left maybeProjectLockData -> return $ ProjectBusy $ (.pid) <$> maybeProjectLockData

makeProjectBusyError :: Maybe WaspProcessId -> CommandError
makeProjectBusyError maybeProcessId =
  CommandError "Wasp project is busy" $
    "Another Wasp command"
      ++ maybe "" (\pid -> " (PID " ++ show pid ++ ")") maybeProcessId
      ++ " is working on this project right now. Run this command again once it finishes."

-- | Given any compile warnings and errors, prints information about how compilation went:
-- reports it as success if there was no errors, or if a failure if there were errors,
-- also shows any warnings (and errors), ... .
-- Normally you will want to call this function after compile step is done and you want
-- to report to user how it went.
printCompilationResult :: ([CompileWarning], [CompileError]) -> IO ()
printCompilationResult (warns, errs) = do
  if null errs
    then cliSendMessage $ Msg.Success "Your wasp project has successfully compiled."
    else printErrorsIfAny errs
  printWarningsIfAny warns

printWarningsAndErrorsIfAny :: ([CompileWarning], [CompileError]) -> IO ()
printWarningsAndErrorsIfAny (warns, errs) = do
  printWarningsIfAny warns
  printErrorsIfAny errs

printWarningsIfAny :: [CompileWarning] -> IO ()
printWarningsIfAny warns = do
  unless (null warns) $
    cliSendMessage $
      Msg.Warning compilationWarningsTitle $
        formatErrorOrWarningMessages warns

printErrorsIfAny :: [CompileError] -> IO ()
printErrorsIfAny errs = do
  unless (null errs) $
    cliSendMessage $
      Msg.Failure "Your wasp project failed to compile" $
        formatErrorOrWarningMessages errs

formatErrorOrWarningMessages :: [String] -> String
formatErrorOrWarningMessages = intercalate "\n" . map ("- " ++)

compilationWarningsTitle :: String
compilationWarningsTitle = "Your wasp project reported following warnings during compilation"

analysisErrorsTitle :: [CompileError] -> String
analysisErrorsTitle errors = "Analyzing wasp project failed, " <> show (length errors) <> " errors found"

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.GeneratedAppDir) ->
  IO ([CompileWarning], Either [CompileError] AS.AppSpec)
compileIO waspProjectDir = compileIOWithOptions (defaultCompileOptions waspProjectDir) waspProjectDir

compileIOWithOptions ::
  CompileOptions ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.GeneratedAppDir) ->
  IO ([CompileWarning], Either [CompileError] AS.AppSpec)
compileIOWithOptions options waspProjectDir outDir =
  Wasp.Project.compile waspProjectDir outDir options

defaultCompileOptions :: Path' Abs (Dir WaspProjectDir) -> CompileOptions
defaultCompileOptions waspProjectDir =
  CompileOptions
    { waspProjectDir,
      buildType = BuildType.Development,
      sendMessage = cliSendMessage,
      generatorWarningsFilter = id
    }

analyze :: Path' Abs (Dir WaspProjectDir) -> Command AS.AppSpec
analyze waspProjectDir = do
  analyzeWithOptions waspProjectDir $ defaultCompileOptions waspProjectDir

-- | Analyzes Wasp project that the current working directory is a part of and returns
-- AppSpec. So same like compilation, but it stops before any code generation.
-- Throws if there were any compilation errors.
analyzeWithOptions :: Path' Abs (Dir WaspProjectDir) -> CompileOptions -> Command AS.AppSpec
analyzeWithOptions waspProjectDir options = do
  (appSpecOrErrors, warnings) <- liftIO $ Wasp.Project.analyzeWaspProject waspProjectDir options
  liftIO $ printWarningsIfAny warnings
  case appSpecOrErrors of
    Left errors ->
      throwError $
        CommandError (analysisErrorsTitle errors) (formatErrorOrWarningMessages errors)
    Right spec -> return spec

-- | Like 'analyze', but keeps stdout free for machine-readable output:
-- compile warnings and errors go to stderr instead ('analyze' prints
-- everything to stdout, via 'cliSendMessage'). Exits with a failure code on
-- compile errors, bypassing 'CommandError' for the same reason.
analyzeWithDiagnosticsOnStderr :: Path' Abs (Dir WaspProjectDir) -> Command AS.AppSpec
analyzeWithDiagnosticsOnStderr waspProjectDir = do
  (appSpecOrErrors, warnings) <-
    liftIO $ Wasp.Project.analyzeWaspProject waspProjectDir $ defaultCompileOptions waspProjectDir
  liftIO $
    unless (null warnings) $
      printDiagnosticToStderr compilationWarningsTitle (formatErrorOrWarningMessages warnings)
  case appSpecOrErrors of
    Right spec -> return spec
    Left errors ->
      liftIO $ do
        printDiagnosticToStderr (analysisErrorsTitle errors) (formatErrorOrWarningMessages errors)
        exitFailure

printDiagnosticToStderr :: String -> String -> IO ()
printDiagnosticToStderr diagnosticTitle body = hPutStrLn stderr $ diagnosticTitle <> ":\n" <> body
