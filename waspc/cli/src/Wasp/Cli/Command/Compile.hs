module Wasp.Cli.Command.Compile
  ( compileIO,
    compile,
    compileWithOptions,
    compileIOWithOptions,
    defaultCompileOptions,
    printCompilationResult,
    printWarningsAndErrorsIfAny,
    analyze,
    analyzeWithOptions,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import StrongPath (Abs, Dir, Path', (</>))
import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Message (cliSendMessage)
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Generator
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning, WaspProjectDir)
import qualified Wasp.Project

-- | Same like 'compileWithOptions', but with default compile options.
compile :: Command [CompileWarning]
compile = do
  -- TODO: Consider a way to remove the redundancy of finding the project root
  -- here and in compileWithOptions. One option could be to add this to defaultCompileOptions
  -- add make externalCodeDirPath a helper function, along with any others we typically need.
  InWaspProject waspProjectDir <- require
  compileWithOptions $ defaultCompileOptions waspProjectDir

-- | Compiles Wasp project that the current working directory is part of.
-- Does all the steps, from analysis to generation, and at the end writes generated code
-- to the disk, to the .wasp dir.
-- At the end, prints a report on how compilation went (by printing warnings, errors,
-- success/failure message, ...).
-- Finally, throws if there was a compile error, otherwise returns any compile warnings.
compileWithOptions :: CompileOptions -> Command [CompileWarning]
compileWithOptions options = do
  InWaspProject waspProjectDir <- require
  let outDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir
          </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Compiling wasp project..."
  (warnings, errors) <- liftIO $ compileIOWithOptions options waspProjectDir outDir

  liftIO $ printCompilationResult (warnings, errors)
  if null errors
    then return warnings
    else
      throwError $
        CommandError "Compilation of wasp project failed" $
          show (length errors) ++ " errors found"

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
      Msg.Warning "Your wasp project reported following warnings during compilation" $
        formatErrorOrWarningMessages warns

printErrorsIfAny :: [CompileError] -> IO ()
printErrorsIfAny errs = do
  unless (null errs) $
    cliSendMessage $
      Msg.Failure "Your wasp project failed to compile" $
        formatErrorOrWarningMessages errs

formatErrorOrWarningMessages :: [String] -> String
formatErrorOrWarningMessages = intercalate "\n" . map ("- " ++)

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.ProjectRootDir) ->
  IO ([CompileWarning], [CompileError])
compileIO waspProjectDir outDir =
  compileIOWithOptions (defaultCompileOptions waspProjectDir) waspProjectDir outDir

compileIOWithOptions ::
  CompileOptions ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.ProjectRootDir) ->
  IO ([CompileWarning], [CompileError])
compileIOWithOptions options waspProjectDir outDir =
  Wasp.Project.compile waspProjectDir outDir options

defaultCompileOptions :: Path' Abs (Dir WaspProjectDir) -> CompileOptions
defaultCompileOptions waspProjectDir =
  CompileOptions
    { externalServerCodeDirPath = waspProjectDir </> Common.extServerCodeDirInWaspProjectDir,
      externalClientCodeDirPath = waspProjectDir </> Common.extClientCodeDirInWaspProjectDir,
      externalSharedCodeDirPath = waspProjectDir </> Common.extSharedCodeDirInWaspProjectDir,
      isBuild = False,
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
  liftIO (Wasp.Project.analyzeWaspProject waspProjectDir options) >>= \case
    Left errors ->
      throwError $
        CommandError "Analyzing wasp project failed" $
          show (length errors) <> " errors found:\n" <> formatErrorOrWarningMessages errors
    Right spec -> return spec
