-- | "Project" here stands for a Wasp source project, and this module offers
-- logic for operating on and processing a Wasp source project, as a whole.
module Wasp.Project
  ( WaspProjectDir,
    CompileResult (..),
    compileResultWarningsAndErrors,
    compile,
    CompileError,
    CompileWarning,
    analyzeWaspProject,
    compileAndRenderDockerfile,
  )
where

import Control.Arrow (ArrowChoice (left), first)
import Data.List.NonEmpty (toList)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import StrongPath (Abs, Dir, Path')
import qualified Wasp.AppSpec as AS
import Wasp.CompileOptions (CompileOptions (generatorWarningsFilter), sendMessage)
import qualified Wasp.Generator as Generator
import qualified Wasp.Generator.DockerGenerator as DockerGenerator
import Wasp.Generator.WriteFileDrafts (GeneratedAppPathChange)
import Wasp.Project.Analyze (analyzeWaspProject)
import Wasp.Project.Common (CompileError, CompileWarning, WaspProjectDir)
import qualified Wasp.Project.Env as Project.Env

data CompileResult = CompileResult
  { _compileWarnings :: [CompileWarning],
    _compileErrors :: [CompileError],
    _compileGeneratedAppPathChanges :: [GeneratedAppPathChange]
  }

compileResultWarningsAndErrors :: CompileResult -> ([CompileWarning], [CompileError])
compileResultWarningsAndErrors compileResult = (_compileWarnings compileResult, _compileErrors compileResult)

compile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Generator.GeneratedAppDir) ->
  CompileOptions ->
  IO CompileResult
compile waspDir outDir options = do
  (compileWarningsAndErrors, generatedAppPathChanges) <-
    analyzeWaspProject waspDir options >>= \case
      (Left analyzerErrors, analyzerWarnings) -> return ((analyzerWarnings, analyzerErrors), [])
      (Right appSpec, analyzerWarnings) ->
        first (first (<> analyzerWarnings)) <$> generateCode appSpec outDir options
  dotEnvWarnings <- maybeToList <$> Project.Env.warnIfTheDotEnvPresent waspDir
  let (warnings, errors) =
        mconcat
          [ compileWarningsAndErrors,
            (dotEnvWarnings, [])
          ]
  return $
    CompileResult
      { _compileWarnings = warnings,
        _compileErrors = errors,
        _compileGeneratedAppPathChanges = generatedAppPathChanges
      }

generateCode ::
  AS.AppSpec ->
  Path' Abs (Dir Generator.GeneratedAppDir) ->
  CompileOptions ->
  IO (([CompileWarning], [CompileError]), [GeneratedAppPathChange])
generateCode appSpec outDir options = do
  (generatorWarnings, generatorErrors, generatedAppPathChanges) <- Generator.writeWebAppCode appSpec outDir (sendMessage options)
  let filteredWarnings = generatorWarningsFilter options generatorWarnings
  return ((show <$> filteredWarnings, show <$> generatorErrors), generatedAppPathChanges)

compileAndRenderDockerfile :: Path' Abs (Dir WaspProjectDir) -> CompileOptions -> IO (Either [CompileError] Text)
compileAndRenderDockerfile waspDir compileOptions = do
  (appSpecOrAnalyzerErrors, _analyzerWarnings) <- analyzeWaspProject waspDir compileOptions
  case appSpecOrAnalyzerErrors of
    Left errors -> return $ Left errors
    Right appSpec -> do
      dockerfileOrGeneratorErrors <- DockerGenerator.compileAndRenderDockerfile appSpec
      return $ Control.Arrow.left (map show . toList) dockerfileOrGeneratorErrors
