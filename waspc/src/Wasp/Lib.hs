module Wasp.Lib
  ( compile,
    Generator.start,
    ProjectRootDir,
    findWaspFile,
    analyzeWaspProject,
    compileAndRenderDockerfile,
  )
where

import Control.Arrow (left)
import Control.Monad.Extra (whenMaybeM)
import Data.List (find, isSuffixOf)
import Data.List.NonEmpty (NonEmpty, fromList, toList)
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
import qualified Wasp.Generator.DockerGenerator as DockerGenerator
import Wasp.Generator.ServerGenerator.Common (dotEnvServer)
import Wasp.Generator.WebAppGenerator.Common (dotEnvClient)
import qualified Wasp.Util.IO as Util.IO

type CompileError = String

type CompileWarning = String

compile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  CompileOptions ->
  IO ([CompileWarning], [CompileError])
compile waspDir outDir options = do
  (analyzerWarnings, appSpecOrAnalyzerErrors) <- analyzeWaspProject waspDir options
  compilerWarningsAndErrors <- case appSpecOrAnalyzerErrors of
    Left analyzerErrors -> return ([], toList analyzerErrors)
    Right appSpec ->
      case ASV.validateAppSpec appSpec of
        [] -> do
          (generatorWarnings, generatorErrors) <- Generator.writeWebAppCode appSpec outDir (sendMessage options)
          return (map show $ generatorWarningsFilter options generatorWarnings, map show generatorErrors)
        validationErrors -> do
          return ([], map show validationErrors)
  return $ (analyzerWarnings, []) <> compilerWarningsAndErrors

analyzeWaspProject ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  IO ([CompileWarning], Either (NonEmpty CompileError) AS.AppSpec)
analyzeWaspProject waspDir options = do
  maybeWaspFilePath <- findWaspFile waspDir
  appSpecOrAnalyzerErrors <- case maybeWaspFilePath of
    Nothing -> return $ Left $ fromList ["Couldn't find a single *.wasp file."]
    Just waspFilePath -> do
      waspFileContent <- readFile (SP.fromAbsFile waspFilePath)
      case Analyzer.analyze waspFileContent of
        Left analyzeError ->
          return $
            Left $
              fromList
                [ showCompilerErrorForTerminal
                    (waspFilePath, waspFileContent)
                    (getErrorMessageAndCtx analyzeError)
                ]
        Right decls -> do
          externalCodeFiles <-
            ExternalCode.readFiles (CompileOptions.externalCodeDirPath options)
          maybeDotEnvServerFile <- findDotEnvServer waspDir
          maybeDotEnvClientFile <- findDotEnvClient waspDir
          maybeMigrationsDir <- findMigrationsDir waspDir
          maybeUserDockerfileContents <- loadUserDockerfileContents waspDir
          return $
            Right
              AS.AppSpec
                { AS.decls = decls,
                  AS.externalCodeFiles = externalCodeFiles,
                  AS.externalCodeDirPath = CompileOptions.externalCodeDirPath options,
                  AS.migrationsDir = maybeMigrationsDir,
                  AS.dotEnvServerFile = maybeDotEnvServerFile,
                  AS.dotEnvClientFile = maybeDotEnvClientFile,
                  AS.isBuild = CompileOptions.isBuild options,
                  AS.userDockerfileContents = maybeUserDockerfileContents
                }
  analyzerWarnings <- warnIfDotEnvPresent waspDir
  return (analyzerWarnings, appSpecOrAnalyzerErrors)

-- | Checks the wasp directory for potential problems, and issues warnings if any are found.
warnIfDotEnvPresent :: Path' Abs (Dir WaspProjectDir) -> IO [CompileWarning]
warnIfDotEnvPresent waspDir = do
  maybeDotEnvFile <- findDotEnv waspDir
  case maybeDotEnvFile of
    Nothing -> return []
    Just _ -> return ["Wasp .env files should be named .env.server or .env.client, depending on their use."]

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findWaspFile waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ (waspDir SP.</>) <$> find isWaspFile files
  where
    isWaspFile path =
      ".wasp" `isSuffixOf` SP.toFilePath path
        && (length (SP.toFilePath path) > length (".wasp" :: String))

findDotEnvServer :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnvServer waspDir = findFileInWaspProjectDir waspDir dotEnvServer

findDotEnvClient :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnvClient waspDir = findFileInWaspProjectDir waspDir dotEnvClient

findDotEnv :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnv waspDir = findFileInWaspProjectDir waspDir [relfile|.env|]

findFileInWaspProjectDir ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (SP.Rel WaspProjectDir) File' ->
  IO (Maybe (Path' Abs File'))
findFileInWaspProjectDir waspDir file = do
  let fileAbsFp = waspDir SP.</> file
  fileExists <- doesFileExist $ SP.toFilePath fileAbsFp
  return $ if fileExists then Just fileAbsFp else Nothing

findMigrationsDir ::
  Path' Abs (Dir WaspProjectDir) ->
  IO (Maybe (Path' Abs (Dir DbMigrationsDir)))
findMigrationsDir waspDir = do
  let migrationsAbsPath = waspDir SP.</> dbMigrationsDirInWaspProjectDir
  migrationsExists <- doesDirectoryExist $ SP.fromAbsDir migrationsAbsPath
  return $ if migrationsExists then Just migrationsAbsPath else Nothing

loadUserDockerfileContents :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe Text)
loadUserDockerfileContents waspDir = do
  let dockerfileAbsPath = SP.toFilePath $ waspDir SP.</> [relfile|Dockerfile|]
  whenMaybeM (doesFileExist dockerfileAbsPath) $ T.IO.readFile dockerfileAbsPath

compileAndRenderDockerfile :: Path' Abs (Dir WaspProjectDir) -> CompileOptions -> IO (Either [CompileError] Text)
compileAndRenderDockerfile waspDir compileOptions = do
  (_, appSpecOrAnalyzerErrors) <- analyzeWaspProject waspDir compileOptions
  case appSpecOrAnalyzerErrors of
    Left errors -> return . Left . toList $ errors
    Right appSpec -> do
      dockerfileOrGeneratorErrors <- DockerGenerator.compileAndRenderDockerfile appSpec
      return $ left (map show . toList) dockerfileOrGeneratorErrors
