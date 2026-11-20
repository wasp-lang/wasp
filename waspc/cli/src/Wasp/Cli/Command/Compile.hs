{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Cli.Command.Compile
  ( CompileResult (..),
    compileIO,
    compileCommand,
    compile,
    compileWithOptions,
    compileIOWithOptions,
    defaultCompileOptions,
    withSetupGoals,
    printCompilationResult,
    printWarningsAndErrorsIfAny,
    analyze,
    analyzeWithOptions,
    analyzeWithDiagnosticsOnStderr,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.IO (hPutStrLn, stderr)
import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Generator
import Wasp.Generator.Setup (SetupGoal, allSetupGoals)
import qualified Wasp.Generator.WaspInfo as WaspInfo
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileResult (..), CompileWarning, WaspProjectDir)
import qualified Wasp.Project
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)
import Wasp.Util.IO (doesDirectoryExist, removeDirectory)

-- | Meant for the standalone `wasp compile` command: commands that hold the
-- project lock themselves should call 'compile' instead.
compileCommand :: Command ([CompileWarning], AS.AppSpec)
compileCommand = ProjectLock.acquireExclusive compile

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
    cliSendMessageC
      $ Msg.Start
      $ "Clearing the content of the " ++ SP.fromRelDir generatedAppDirInWaspProjectDir ++ " directory..."
    liftIO $ removeDirectory outDir
    cliSendMessageC
      $ Msg.Success
      $ "Successfully cleared the contents of the " ++ SP.fromRelDir generatedAppDirInWaspProjectDir ++ " directory."

  cliSendMessageC $ Msg.Start "Compiling wasp project..."
  compileResult <- liftIO $ compileIOWithOptions options waspProjectDir outDir

  liftIO $ printCompilationResult compileResult
  case _compileOutcome compileResult of
    Right appSpec -> return (_compileWarnings compileResult, appSpec)
    Left _ -> throwError CommandAborted

-- | Given any compile warnings and errors, prints information about how compilation went:
-- reports it as success if there was no errors, or if a failure if there were errors,
-- also shows any warnings (and errors), ... .
-- Normally you will want to call this function after compile step is done and you want
-- to report to user how it went.
printCompilationResult :: CompileResult -> IO ()
printCompilationResult compileResult = do
  case _compileOutcome compileResult of
    Right _ -> cliSendMessage $ Msg.Success "Your wasp project has successfully compiled."
    Left errs -> printErrorsIfAny errs
  printWarningsIfAny $ _compileWarnings compileResult

printWarningsAndErrorsIfAny :: ([CompileWarning], [CompileError]) -> IO ()
printWarningsAndErrorsIfAny (warns, errs) = do
  printWarningsIfAny warns
  printErrorsIfAny errs

printWarningsIfAny :: [CompileWarning] -> IO ()
printWarningsIfAny warns = do
  unless (null warns)
    $ cliSendMessage
    $ Msg.Warning warns

printErrorsIfAny :: [CompileError] -> IO ()
printErrorsIfAny errs = do
  unless (null errs)
    $ cliSendMessage
    $ Msg.Failure errs

compilationWarningsTitle :: String
compilationWarningsTitle = "Your wasp project reported following warnings during compilation"

analysisErrorsTitle :: [CompileError] -> String
analysisErrorsTitle errors = "Analyzing wasp project failed, " <> show (length errors) <> " errors found"

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.GeneratedAppDir) ->
  IO CompileResult
compileIO waspProjectDir outDir =
  compileIOWithOptions (defaultCompileOptions waspProjectDir) waspProjectDir outDir

compileIOWithOptions ::
  CompileOptions ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.GeneratedAppDir) ->
  IO CompileResult
compileIOWithOptions options waspProjectDir outDir =
  Wasp.Project.compile waspProjectDir outDir options

defaultCompileOptions :: Path' Abs (Dir WaspProjectDir) -> CompileOptions
defaultCompileOptions waspProjectDir =
  CompileOptions
    { waspProjectDir,
      buildType = BuildType.Development,
      sendMessage = cliSendMessage,
      setupGoals = allSetupGoals
    }

withSetupGoals :: [SetupGoal] -> CompileOptions -> CompileOptions
withSetupGoals goals options = options {setupGoals = goals}

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
    Left [] -> throwError CommandAborted
    Left errors -> do
      liftIO $ cliSendMessage $ Msg.Failure errors
      throwError CommandAborted
    Right spec -> return spec

-- | Like 'analyze', but keeps stdout free for machine-readable output:
-- compile warnings and errors go to stderr instead ('analyze' prints
-- everything to stdout, via 'cliSendMessage'). Exits with a failure code on
-- compile errors, bypassing 'CommandError' for the same reason.
analyzeWithDiagnosticsOnStderr :: Path' Abs (Dir WaspProjectDir) -> Command AS.AppSpec
analyzeWithDiagnosticsOnStderr waspProjectDir = do
  (appSpecOrErrors, warnings) <-
    liftIO $ Wasp.Project.analyzeWaspProject waspProjectDir $ defaultCompileOptions waspProjectDir
  liftIO
    $ unless (null warnings)
    $ printDiagnosticToStderr compilationWarningsTitle warnings
  case appSpecOrErrors of
    Right spec -> return spec
    Left [] -> throwError CommandAborted
    Left errors -> do
      liftIO $ printDiagnosticToStderr (analysisErrorsTitle errors) errors
      throwError CommandAborted

printDiagnosticToStderr :: String -> [Msg.Entry] -> IO ()
printDiagnosticToStderr diagnosticTitle entries = hPutStrLn stderr $ diagnosticTitle <> ":\n" <> intercalate "\n\n" (map plainText entries)
  where
    plainText (Msg.Entry text parts) = intercalate "\n" $ filter (not . null) $ text : map plainPart parts
    plainPart (Msg.Text text) = text
    plainPart (Msg.ToolOutput command exitCode output) = "Output of " <> command <> " (exit code " <> show exitCode <> "):\n" <> output
