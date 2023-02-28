-- | "Project" here stands for a Wasp source project, and this module offers
-- logic for operating on and processing a Wasp source project, as a whole.
module Wasp.Project
  ( WaspProjectDir,
    compile,
    CompileError,
    CompileWarning,
    analyzeWaspProject,
    compileAndRenderDockerfile,
  )
where

import Control.Arrow (ArrowChoice (left))
import Data.List.NonEmpty (toList)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import StrongPath (Abs, Dir, Path')
import qualified Wasp.AppSpec as AS
import Wasp.CompileOptions (CompileOptions (generatorWarningsFilter), sendMessage)
import qualified Wasp.Generator as Generator
import qualified Wasp.Generator.DockerGenerator as DockerGenerator
import Wasp.Project.Analyze (analyzeWaspProject)
import Wasp.Project.Common (CompileError, CompileWarning, WaspProjectDir)
import qualified Wasp.Project.Env as Project.Env

compile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Generator.ProjectRootDir) ->
  CompileOptions ->
  IO ([CompileWarning], [CompileError])
compile waspDir outDir options = do
  compileWarningsAndErrors <-
    analyzeWaspProject waspDir options >>= \case
      Left analyzerErrors -> return ([], analyzerErrors)
      Right appSpec -> generateCode appSpec outDir options
  dotEnvWarnings <- maybeToList <$> Project.Env.warnIfTheDotEnvPresent waspDir
  return $ (dotEnvWarnings, []) <> compileWarningsAndErrors

generateCode ::
  AS.AppSpec ->
  Path' Abs (Dir Generator.ProjectRootDir) ->
  CompileOptions ->
  IO ([CompileError], [CompileWarning])
generateCode appSpec outDir options = do
  (generatorWarnings, generatorErrors) <- Generator.writeWebAppCode appSpec outDir (sendMessage options)
  let filteredWarnings = generatorWarningsFilter options generatorWarnings
  return (show <$> filteredWarnings, show <$> generatorErrors)

compileAndRenderDockerfile :: Path' Abs (Dir WaspProjectDir) -> CompileOptions -> IO (Either [CompileError] Text)
compileAndRenderDockerfile waspDir compileOptions = do
  appSpecOrAnalyzerErrors <- analyzeWaspProject waspDir compileOptions
  case appSpecOrAnalyzerErrors of
    Left errors -> return $ Left errors
    Right appSpec -> do
      dockerfileOrGeneratorErrors <- DockerGenerator.compileAndRenderDockerfile appSpec
      return $ Control.Arrow.left (map show . toList) dockerfileOrGeneratorErrors
