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
    reportCompilationResult,
    reportWarningsAndErrorsIfAny,
    analyze,
    analyzeWithOptions,
    analyzeWithDiagnosticsOnStderr,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Exit (exitFailure)
import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, CommandError (..), getConsole, require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Console (Console (..))
import Wasp.Cli.Console.Reporter (Reporter (..))
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Generator
import Wasp.Generator.Setup (SetupGoal, allSetupGoals)
import qualified Wasp.Generator.WaspInfo as WaspInfo
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileResult (..), CompileWarning, WaspProjectDir, compileResultWarningsAndErrors)
import qualified Wasp.Project
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)
import Wasp.Util.IO (doesDirectoryExist, removeDirectory)
import Wasp.Util.IO.Process (OutputStream (..), WriteOutput)

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
  console <- getConsole
  compileWithOptions $ defaultCompileOptions console waspProjectDir

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
  let (warnings, errors) = compileResultWarningsAndErrors compileResult

  console <- getConsole
  liftIO $ reportCompilationResult console.reporter (warnings, errors)
  case _compileOutcome compileResult of
    Right appSpec -> return (warnings, appSpec)
    Left compileErrors ->
      throwError
        $ CommandError "Compilation of wasp project failed"
        $ show (length compileErrors) ++ " errors found"

-- | Given any compile warnings and errors, prints information about how compilation went:
-- reports it as success if there was no errors, or if a failure if there were errors,
-- also shows any warnings (and errors), ... .
-- Normally you will want to call this function after compile step is done and you want
-- to report to user how it went.
reportCompilationResult :: Reporter -> ([CompileWarning], [CompileError]) -> IO ()
reportCompilationResult reporter (warns, errs) = do
  if null errs
    then reporter.send $ Msg.Success "Your wasp project has successfully compiled."
    else reportErrorsIfAny reporter errs
  reportWarningsIfAny reporter warns

reportWarningsAndErrorsIfAny :: Reporter -> ([CompileWarning], [CompileError]) -> IO ()
reportWarningsAndErrorsIfAny reporter (warns, errs) = do
  reportWarningsIfAny reporter warns
  reportErrorsIfAny reporter errs

reportWarningsIfAny :: Reporter -> [CompileWarning] -> IO ()
reportWarningsIfAny reporter warns = do
  unless (null warns)
    $ reporter.send
    $ Msg.Warning compilationWarningsTitle
    $ formatErrorOrWarningMessages warns

reportErrorsIfAny :: Reporter -> [CompileError] -> IO ()
reportErrorsIfAny reporter errs = do
  unless (null errs)
    $ reporter.send
    $ Msg.Failure "Your wasp project failed to compile"
    $ formatErrorOrWarningMessages errs

formatErrorOrWarningMessages :: [String] -> String
formatErrorOrWarningMessages = intercalate "\n" . map ("- " ++)

compilationWarningsTitle :: String
compilationWarningsTitle = "Your wasp project reported following warnings during compilation"

analysisErrorsTitle :: [CompileError] -> String
analysisErrorsTitle errors = "Analyzing wasp project failed, " <> show (length errors) <> " errors found"

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO ::
  Console ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.GeneratedAppDir) ->
  IO CompileResult
compileIO console waspProjectDir outDir =
  compileIOWithOptions (defaultCompileOptions console waspProjectDir) waspProjectDir outDir

compileIOWithOptions ::
  CompileOptions ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.GeneratedAppDir) ->
  IO CompileResult
compileIOWithOptions options waspProjectDir outDir =
  Wasp.Project.compile waspProjectDir outDir options

defaultCompileOptions :: Console -> Path' Abs (Dir WaspProjectDir) -> CompileOptions
defaultCompileOptions console waspProjectDir =
  CompileOptions
    { waspProjectDir,
      buildType = BuildType.Development,
      sendMessage = console.reporter.send,
      writeJobOutput = console.writeRawOutput,
      setupGoals = allSetupGoals
    }

withSetupGoals :: [SetupGoal] -> CompileOptions -> CompileOptions
withSetupGoals goals options = options {setupGoals = goals}

analyze :: Path' Abs (Dir WaspProjectDir) -> Command AS.AppSpec
analyze waspProjectDir = do
  console <- getConsole
  analyzeWithOptions waspProjectDir $ defaultCompileOptions console waspProjectDir

-- | Analyzes Wasp project that the current working directory is a part of and returns
-- AppSpec. So same like compilation, but it stops before any code generation.
-- Throws if there were any compilation errors.
analyzeWithOptions :: Path' Abs (Dir WaspProjectDir) -> CompileOptions -> Command AS.AppSpec
analyzeWithOptions waspProjectDir options = do
  (appSpecOrErrors, warnings) <- liftIO $ Wasp.Project.analyzeWaspProject waspProjectDir options
  console <- getConsole
  liftIO $ reportWarningsIfAny console.reporter warnings
  case appSpecOrErrors of
    Left errors ->
      throwError $
        CommandError (analysisErrorsTitle errors) (formatErrorOrWarningMessages errors)
    Right spec -> return spec

-- | Like 'analyze', but keeps stdout free for machine-readable output:
-- compile warnings and errors go to stderr instead ('analyze'
-- sends them as messages, through the reporter). Exits with a failure code on
-- compile errors, bypassing 'CommandError' for the same reason.
analyzeWithDiagnosticsOnStderr :: Path' Abs (Dir WaspProjectDir) -> Command AS.AppSpec
analyzeWithDiagnosticsOnStderr waspProjectDir = do
  console <- getConsole
  (appSpecOrErrors, warnings) <-
    liftIO $ Wasp.Project.analyzeWaspProject waspProjectDir $ defaultCompileOptions console waspProjectDir
  liftIO
    $ unless (null warnings)
    $ printDiagnosticToStderr console.writeRawOutput compilationWarningsTitle (formatErrorOrWarningMessages warnings)
  case appSpecOrErrors of
    Right spec -> return spec
    Left errors ->
      liftIO $ do
        printDiagnosticToStderr console.writeRawOutput (analysisErrorsTitle errors) (formatErrorOrWarningMessages errors)
        exitFailure

printDiagnosticToStderr :: WriteOutput -> String -> String -> IO ()
printDiagnosticToStderr writeRawOutput diagnosticTitle body =
  writeRawOutput Stderr . T.pack $ diagnosticTitle <> ":\n" <> body <> "\n"
