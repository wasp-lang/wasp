module Wasp.Lib
  ( compile,
    Generator.start,
    ProjectRootDir,
    findWaspFile,
    analyzeProject,
    compileAndRenderDockerfile,
  )
where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.Extra (whenMaybeM)
import Data.List (find, isSuffixOf)
import Data.List.NonEmpty (toList)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, Dir, File', Path', Rel, fromAbsDir, fromAbsFile, relfile, toFilePath, (</>))
import System.Directory (doesDirectoryExist, doesFileExist)
import qualified Wasp.Analyzer as Analyzer
import Wasp.Analyzer.AnalyzeError (getErrorMessageAndCtx)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Valid (validateAppSpec)
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
import Wasp.Util (maybeToEither)
import qualified Wasp.Util.IO as Util.IO

type CompileError = String

type CompileWarning = String

compile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  CompileOptions ->
  IO ([CompileWarning], [CompileError])
compile waspDir outDir options = do
  compileWarnings <- maybeToList <$> warnIfDotEnvPresent waspDir
  appSpecOrCompileErrors <- analyzeProject waspDir options
  compileWarningsAndErrors <- case appSpecOrCompileErrors of
    Left analyzerErrors -> return (compileWarnings, analyzerErrors)
    Right appSpec -> generateCode appSpec outDir options
  return $ (compileWarnings, []) <> compileWarningsAndErrors

analyzeProject ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  IO (Either [CompileError] AS.AppSpec)
analyzeProject waspDir options = runExceptT $ do
  waspFilePath <- ExceptT $ left pure <$> findWaspFile waspDir
  declarations <- ExceptT $ left pure <$> analyzeWaspFileContent waspFilePath
  ExceptT $ constructAppSpec waspDir options declarations

generateCode ::
  AS.AppSpec ->
  Path' Abs (Dir ProjectRootDir) ->
  CompileOptions ->
  IO ([CompileError], [CompileWarning])
generateCode appSpec outDir options = do
  (generatorWarnings, generatorErrors) <- Generator.writeWebAppCode appSpec outDir (sendMessage options)
  let filteredWarnings = generatorWarningsFilter options generatorWarnings
  return (map show filteredWarnings, map show generatorErrors)

-- | Checks the wasp directory for potential problems, and issues warnings if any are found.
warnIfDotEnvPresent :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe CompileWarning)
warnIfDotEnvPresent waspDir = (warningMessage <$) <$> findDotEnv waspDir
  where
    warningMessage = "Wasp .env files should be named .env.server or .env.client, depending on their use."

analyzeWaspFileContent :: Path' Abs File' -> IO (Either CompileError [AS.Decl])
analyzeWaspFileContent waspFilePath = do
  waspFileContent <- readFile (fromAbsFile waspFilePath)
  let declsOrAnalyzeError = Analyzer.analyze waspFileContent
  return $
    left
      (showCompilerErrorForTerminal (waspFilePath, waspFileContent) . getErrorMessageAndCtx)
      declsOrAnalyzeError

constructAppSpec ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  [AS.Decl] ->
  IO (Either [CompileError] AS.AppSpec)
constructAppSpec waspDir options decls = do
  externalServerCodeFiles <-
    ExternalCode.readFiles (CompileOptions.externalServerCodeDirPath options)
  externalClientCodeFiles <-
    ExternalCode.readFiles (CompileOptions.externalClientCodeDirPath options)
  maybeDotEnvServerFile <- findDotEnvServer waspDir
  maybeDotEnvClientFile <- findDotEnvClient waspDir
  maybeMigrationsDir <- findMigrationsDir waspDir
  maybeUserDockerfileContents <- loadUserDockerfileContents waspDir
  let appSpec =
        AS.AppSpec
          { AS.decls = decls,
            AS.externalClientFiles = externalClientCodeFiles,
            AS.externalServerFiles = externalServerCodeFiles,
            AS.migrationsDir = maybeMigrationsDir,
            AS.dotEnvServerFile = maybeDotEnvServerFile,
            AS.dotEnvClientFile = maybeDotEnvClientFile,
            AS.isBuild = CompileOptions.isBuild options,
            AS.userDockerfileContents = maybeUserDockerfileContents
          }
  return $ case validateAppSpec appSpec of
    [] -> Right appSpec
    validationErrors -> Left $ map show validationErrors

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String (Path' Abs File'))
findWaspFile waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ maybeToEither "Couldn't find a single *.wasp file." $ (waspDir </>) <$> find isWaspFile files
  where
    isWaspFile path =
      ".wasp" `isSuffixOf` toFilePath path
        && (length (toFilePath path) > length (".wasp" :: String))

findDotEnvServer :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnvServer waspDir = findFileInWaspProjectDir waspDir dotEnvServer

findDotEnvClient :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnvClient waspDir = findFileInWaspProjectDir waspDir dotEnvClient

findDotEnv :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnv waspDir = findFileInWaspProjectDir waspDir [relfile|.env|]

findFileInWaspProjectDir ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  IO (Maybe (Path' Abs File'))
findFileInWaspProjectDir waspDir file = do
  let fileAbsFp = waspDir </> file
  fileExists <- doesFileExist $ toFilePath fileAbsFp
  return $ if fileExists then Just fileAbsFp else Nothing

findMigrationsDir ::
  Path' Abs (Dir WaspProjectDir) ->
  IO (Maybe (Path' Abs (Dir DbMigrationsDir)))
findMigrationsDir waspDir = do
  let migrationsAbsPath = waspDir </> dbMigrationsDirInWaspProjectDir
  migrationsExists <- doesDirectoryExist $ fromAbsDir migrationsAbsPath
  return $ if migrationsExists then Just migrationsAbsPath else Nothing

loadUserDockerfileContents :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe Text)
loadUserDockerfileContents waspDir = do
  let dockerfileAbsPath = toFilePath $ waspDir </> [relfile|Dockerfile|]
  whenMaybeM (doesFileExist dockerfileAbsPath) $ T.IO.readFile dockerfileAbsPath

compileAndRenderDockerfile :: Path' Abs (Dir WaspProjectDir) -> CompileOptions -> IO (Either [CompileError] Text)
compileAndRenderDockerfile waspDir compileOptions = do
  appSpecOrAnalyzerErrors <- analyzeProject waspDir compileOptions
  case appSpecOrAnalyzerErrors of
    Left errors -> return $ Left errors
    Right appSpec -> do
      dockerfileOrGeneratorErrors <- DockerGenerator.compileAndRenderDockerfile appSpec
      return $ left (map show . toList) dockerfileOrGeneratorErrors
