-- | "Project" here stands for a Wasp source project, and this module offers
-- logic for operating on and processing a Wasp source project, as a whole.
module Wasp.Project
  ( WaspProjectDir,
    Compilation (..),
    prepareCompilation,
    performCompilation,
    isGeneratedAppUpToDate,
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
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Project.Analyze (analyzeWaspProject)
import Wasp.Project.Common (CompileError, CompileWarning, WaspProjectDir)
import qualified Wasp.Project.Env as Project.Env

-- | The result of analyzing and generating a project in memory, before
-- anything is written to the generated app directory.
data Compilation = Compilation
  { appSpec :: AS.AppSpec,
    fileDrafts :: [FileDraft]
  }

-- | Analyzes the Wasp project and generates its file drafts without touching
-- the generated app directory.
prepareCompilation ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  IO ([CompileWarning], Either [CompileError] Compilation)
prepareCompilation waspDir options = do
  (appSpecOrAnalyzerErrors, analyzerWarnings) <- analyzeWaspProject waspDir options
  let (compileWarnings, compilationOrErrors) =
        case appSpecOrAnalyzerErrors of
          Left analyzerErrors -> (analyzerWarnings, Left analyzerErrors)
          Right appSpec ->
            let (generatorWarnings, fileDraftsOrGeneratorErrors) = Generator.generateWebAppCode appSpec
                filteredGeneratorWarnings = generatorWarningsFilter options generatorWarnings
             in ( (show <$> filteredGeneratorWarnings) <> analyzerWarnings,
                  left (map show) $ Compilation appSpec <$> fileDraftsOrGeneratorErrors
                )
  dotEnvWarnings <- maybeToList <$> Project.Env.warnIfTheDotEnvPresent waspDir
  return (compileWarnings <> dotEnvWarnings, compilationOrErrors)

-- | Writes a prepared compilation to disk and runs the generated app setup.
performCompilation ::
  Compilation ->
  Path' Abs (Dir Generator.GeneratedAppDir) ->
  CompileOptions ->
  IO ([CompileWarning], [CompileError])
performCompilation Compilation {appSpec, fileDrafts} outDir options = do
  (generatorWarnings, generatorErrors) <-
    Generator.writeWebAppCode appSpec outDir fileDrafts (sendMessage options)
  let filteredWarnings = generatorWarningsFilter options generatorWarnings
  return (show <$> filteredWarnings, show <$> generatorErrors)

-- | Returns 'True' if performing the compilation would change nothing on disk.
isGeneratedAppUpToDate :: Compilation -> Path' Abs (Dir Generator.GeneratedAppDir) -> IO Bool
isGeneratedAppUpToDate Compilation {appSpec, fileDrafts} outDir =
  Generator.isGeneratedAppUpToDate appSpec outDir fileDrafts

compile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Generator.GeneratedAppDir) ->
  CompileOptions ->
  IO ([CompileWarning], Either [CompileError] AS.AppSpec)
compile waspDir outDir options = do
  (prepareWarnings, compilationOrErrors) <- prepareCompilation waspDir options
  case compilationOrErrors of
    Left errors -> return (prepareWarnings, Left errors)
    Right compilation -> do
      (performWarnings, errors) <- performCompilation compilation outDir options
      return
        ( prepareWarnings <> performWarnings,
          if null errors then Right compilation.appSpec else Left errors
        )

compileAndRenderDockerfile :: Path' Abs (Dir WaspProjectDir) -> CompileOptions -> IO (Either [CompileError] Text)
compileAndRenderDockerfile waspDir compileOptions = do
  (appSpecOrAnalyzerErrors, _analyzerWarnings) <- analyzeWaspProject waspDir compileOptions
  case appSpecOrAnalyzerErrors of
    Left errors -> return $ Left errors
    Right appSpec -> do
      dockerfileOrGeneratorErrors <- DockerGenerator.compileAndRenderDockerfile appSpec
      return $ Control.Arrow.left (map show . toList) dockerfileOrGeneratorErrors
