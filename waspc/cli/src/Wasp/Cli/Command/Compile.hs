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
    describeCompileEnd,
    analyze,
    analyzeWithOptions,
    analyzeWithWarningsOnStderr,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, compileCommandError, getConsole, reportCompileWarnings, require)
import Wasp.Cli.Command.Message (runTaskC, runTaskWithResultC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Console (Console (..))
import Wasp.Cli.Console.Reporter (Reporter (..), TaskEnd (..), runTaskWithFixedTexts)
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Generator
import Wasp.Generator.Setup (SetupGoal, allSetupGoals)
import qualified Wasp.Generator.WaspInfo as WaspInfo
import Wasp.Problem (Problem (..))
import Wasp.Project (CompileError, CompileResult (..), CompileWarning, WaspProjectDir, compileResultWarningsAndErrors)
import qualified Wasp.Project
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)
import Wasp.TaskRunner (TaskRunner (..))
import Wasp.Util.IO (doesDirectoryExist, removeDirectory)
import Wasp.Util.IO.Process (OutputStream (..), WriteOutput)
import Wasp.Util.TextStyle (styleLiteral)

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
  compileWithOptions $ defaultCompileOptions console.reporter waspProjectDir

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
      outDirName = styleLiteral $ SP.fromRelDir generatedAppDirInWaspProjectDir

  generatedAppIsCompatible <-
    liftIO $ buildType options `WaspInfo.isCompatibleWithExistingBuildAt` outDir

  outDirExists <- liftIO $ doesDirectoryExist outDir

  when (outDirExists && not generatedAppIsCompatible)
    $ runTaskC
      ("Clearing the " ++ outDirName ++ " directory")
      ("Cleared the " ++ outDirName ++ " directory")
      ("Failed to clear the " ++ outDirName ++ " directory")
    $ liftIO
    $ removeDirectory outDir

  compileResult <-
    runTaskWithResultC "Compiling the project" (describeCompileEnd "Compiled the project" compilationFailedText) compilationFailedText
      $ liftIO
      $ compileIOWithOptions options waspProjectDir outDir
  let (warnings, errors) = compileResultWarningsAndErrors compileResult
  case _compileOutcome compileResult of
    Left _ -> throwError $ compileCommandError compilationFailedText errors warnings
    Right appSpec -> do
      console <- getConsole
      liftIO $ reportCompileWarnings console.reporter warnings
      return (warnings, appSpec)
  where
    compilationFailedText = "Compilation failed"

-- | Says how a compile ended on the line that ends its task, together with
-- the number of errors and warnings it had.
describeCompileEnd :: String -> String -> CompileResult -> TaskEnd
describeCompileEnd successText failedText compileResult = case _compileOutcome compileResult of
  Right _ -> TaskSucceeded $ successText `withCountsOf` compileResult
  Left _ -> TaskFailed $ failedText `withCountsOf` compileResult

withCountsOf :: String -> CompileResult -> String
withCountsOf text compileResult = case mapMaybe showCount [(length errors, "error"), (length warnings, "warning")] of
  [] -> text
  counts -> text ++ ": " ++ intercalate ", " counts
  where
    (warnings, errors) = compileResultWarningsAndErrors compileResult

    showCount :: (Int, String) -> Maybe String
    showCount (0, _) = Nothing
    showCount (1, name) = Just $ "1 " ++ name
    showCount (count, name) = Just $ show count ++ " " ++ name ++ "s"

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO ::
  Reporter ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.GeneratedAppDir) ->
  IO CompileResult
compileIO reporter waspProjectDir outDir =
  compileIOWithOptions (defaultCompileOptions reporter waspProjectDir) waspProjectDir outDir

compileIOWithOptions ::
  CompileOptions ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.GeneratedAppDir) ->
  IO CompileResult
compileIOWithOptions options waspProjectDir outDir =
  Wasp.Project.compile waspProjectDir outDir options

defaultCompileOptions :: Reporter -> Path' Abs (Dir WaspProjectDir) -> CompileOptions
defaultCompileOptions reporter waspProjectDir =
  CompileOptions
    { waspProjectDir,
      buildType = BuildType.Development,
      taskRunner = TaskRunner (runTaskWithFixedTexts reporter),
      setupGoals = allSetupGoals
    }

withSetupGoals :: [SetupGoal] -> CompileOptions -> CompileOptions
withSetupGoals goals options = options {setupGoals = goals}

analyze :: Path' Abs (Dir WaspProjectDir) -> Command AS.AppSpec
analyze waspProjectDir = do
  console <- getConsole
  analyzeWithOptions waspProjectDir $ defaultCompileOptions console.reporter waspProjectDir

-- | Analyzes Wasp project that the current working directory is a part of and returns
-- AppSpec. So same like compilation, but it stops before any code generation.
-- Throws if there were any compilation errors.
analyzeWithOptions :: Path' Abs (Dir WaspProjectDir) -> CompileOptions -> Command AS.AppSpec
analyzeWithOptions waspProjectDir options = do
  (appSpecOrErrors, warnings) <- liftIO $ Wasp.Project.analyzeWaspProject waspProjectDir options
  console <- getConsole
  liftIO $ reportCompileWarnings console.reporter warnings
  throwIfAnalysisFailed appSpecOrErrors

-- | Like 'analyze', but keeps the compile warnings out of stdout, which is
-- left for machine-readable output: they go to stderr instead ('analyze'
-- sends them as messages, through the reporter).
analyzeWithWarningsOnStderr :: Path' Abs (Dir WaspProjectDir) -> Command AS.AppSpec
analyzeWithWarningsOnStderr waspProjectDir = do
  console <- getConsole
  (appSpecOrErrors, warnings) <-
    liftIO $ Wasp.Project.analyzeWaspProject waspProjectDir $ defaultCompileOptions console.reporter waspProjectDir
  liftIO
    $ unless (null warnings)
    $ printDiagnosticToStderr console.writeRawOutput compilationWarningsTitle (formatWarnings warnings)
  throwIfAnalysisFailed appSpecOrErrors
  where
    compilationWarningsTitle = "Your wasp project reported following warnings during compilation"
    formatWarnings = intercalate "\n" . map (("- " ++) . (.description))

-- | The warnings are not a part of the error, because they have already been
-- reported.
throwIfAnalysisFailed :: Either [CompileError] AS.AppSpec -> Command AS.AppSpec
throwIfAnalysisFailed = either (\errors -> throwError $ compileCommandError "Analysis of the project failed" errors []) return

printDiagnosticToStderr :: WriteOutput -> String -> String -> IO ()
printDiagnosticToStderr writeRawOutput diagnosticTitle body =
  writeRawOutput Stderr . T.pack $ diagnosticTitle <> ":\n" <> body <> "\n"
