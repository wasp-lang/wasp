module Wasp.Lib
  ( compile,
    Generator.start,
    ProjectRootDir,
    findWaspFile,
    analyzeWaspProject,
    compileAndRenderDockerfileTemplate,
  )
where

import Control.Monad.Extra (whenMaybeM)
import qualified Data.Aeson as Aeson
import Data.List (find, isSuffixOf)
import Data.List.NonEmpty (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, Dir, File', Path', relfile)
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, doesFileExist)
import qualified Wasp.Analyzer as Analyzer
import Wasp.Analyzer.AnalyzeError (getErrorMessageAndCtx)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Common (DbMigrationsDir, WaspProjectDir, dbMigrationsDirInWaspProjectDir)
import Wasp.CompileOptions (CompileOptions (generatorWarningsFilter), sendMessage)
import qualified Wasp.CompileOptions as CompileOptions
import Wasp.Error (showCompilerErrorForTerminal)
import qualified Wasp.ExternalCode as ExternalCode
import qualified Wasp.Generator as Generator
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DockerGenerator (genDockerfile)
import Wasp.Generator.FileDraft (FileDraft (..))
import qualified Wasp.Generator.FileDraft.TemplateFileDraft as TmplFD
import Wasp.Generator.Monad (runGenerator)
import Wasp.Generator.Templates (compileAndRenderTemplate)
import qualified Wasp.Util.IO as Util.IO

type CompileError = String

type CompileWarning = String

compile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  CompileOptions ->
  IO ([CompileWarning], [CompileError])
compile waspDir outDir options = do
  appSpecOrCompileErrors <- analyzeWaspProject waspDir options
  case appSpecOrCompileErrors of
    Left compileErrors -> return ([], compileErrors)
    Right appSpec ->
      case ASV.validateAppSpec appSpec of
        [] -> do
          (generatorWarnings, generatorErrors) <- Generator.writeWebAppCode appSpec outDir (sendMessage options)
          return (map show $ generatorWarningsFilter options generatorWarnings, map show generatorErrors)
        validationErrors -> do
          return ([], map show validationErrors)

analyzeWaspProject ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  IO (Either [CompileError] AS.AppSpec)
analyzeWaspProject waspDir options = do
  maybeWaspFilePath <- findWaspFile waspDir
  case maybeWaspFilePath of
    Nothing -> return $ Left ["Couldn't find a single *.wasp file."]
    Just waspFilePath -> do
      waspFileContent <- readFile (SP.fromAbsFile waspFilePath)
      case Analyzer.analyze waspFileContent of
        Left analyzeError ->
          return $
            Left
              [ showCompilerErrorForTerminal
                  (waspFilePath, waspFileContent)
                  (getErrorMessageAndCtx analyzeError)
              ]
        Right decls -> do
          externalCodeFiles <-
            ExternalCode.readFiles (CompileOptions.externalCodeDirPath options)
          maybeDotEnvFile <- findDotEnvFile waspDir
          maybeMigrationsDir <- findMigrationsDir waspDir
          maybeDockerfileContents <- loadDockerfileContents waspDir
          return $
            Right
              AS.AppSpec
                { AS.decls = decls,
                  AS.externalCodeFiles = externalCodeFiles,
                  AS.externalCodeDirPath = CompileOptions.externalCodeDirPath options,
                  AS.migrationsDir = maybeMigrationsDir,
                  AS.dotEnvFile = maybeDotEnvFile,
                  AS.isBuild = CompileOptions.isBuild options,
                  AS.dockerfileContents = maybeDockerfileContents
                }

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findWaspFile waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ (waspDir SP.</>) <$> find isWaspFile files
  where
    isWaspFile path =
      ".wasp" `isSuffixOf` SP.toFilePath path
        && (length (SP.toFilePath path) > length (".wasp" :: String))

findDotEnvFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnvFile waspDir = do
  let dotEnvAbsPath = waspDir SP.</> [relfile|.env|]
  dotEnvExists <- doesFileExist (SP.toFilePath dotEnvAbsPath)
  return $ if dotEnvExists then Just dotEnvAbsPath else Nothing

findMigrationsDir ::
  Path' Abs (Dir WaspProjectDir) ->
  IO (Maybe (Path' Abs (Dir DbMigrationsDir)))
findMigrationsDir waspDir = do
  let migrationsAbsPath = waspDir SP.</> dbMigrationsDirInWaspProjectDir
  migrationsExists <- doesDirectoryExist $ SP.fromAbsDir migrationsAbsPath
  return $ if migrationsExists then Just migrationsAbsPath else Nothing

loadDockerfileContents :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe Text)
loadDockerfileContents waspDir = do
  let dockerfileAbsPath = SP.toFilePath $ waspDir SP.</> [relfile|Dockerfile|]
  whenMaybeM (doesFileExist dockerfileAbsPath) $ T.IO.readFile dockerfileAbsPath

compileAndRenderDockerfileTemplate :: Path' Abs (Dir WaspProjectDir) -> CompileOptions -> IO (Either [CompileError] Text)
compileAndRenderDockerfileTemplate waspDir compileOptions = do
  appSpecOrCompileErrors <- analyzeWaspProject waspDir compileOptions
  case appSpecOrCompileErrors of
    Left errors -> return $ Left errors
    Right appSpec -> do
      let (_, generatorResult) = runGenerator $ genDockerfile appSpec
      case generatorResult of
        Left generatorErrors -> return . Left $ map show (toList generatorErrors)
        Right (FileDraftTemplateFd draft) -> do
          content <- compileAndRenderTemplate (TmplFD._srcPathInTmplDir draft) (fromMaybe (Aeson.object []) (TmplFD._tmplData draft))
          return $ Right content
        Right _ -> error "Attempted to display Dockerfile, but it was not a Template FileDraft!"
