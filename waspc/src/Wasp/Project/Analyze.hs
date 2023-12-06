module Wasp.Project.Analyze
  ( analyzeWaspProject,
  )
where

import Control.Arrow (ArrowChoice (left))
import Data.List (find, isSuffixOf)
import StrongPath (Abs, Dir, File', Path', toFilePath, (</>))
import qualified Wasp.Analyzer as Analyzer
import Wasp.Analyzer.AnalyzeError (getErrorMessageAndCtx)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Valid (isValidationError, isValidationWarning, validateAppSpec)
import Wasp.CompileOptions (CompileOptions)
import qualified Wasp.CompileOptions as CompileOptions
import qualified Wasp.ConfigFile as CF
import Wasp.Error (showCompilerErrorForTerminal)
import qualified Wasp.ExternalCode as ExternalCode
import qualified Wasp.Generator.ConfigFile as G.CF
import Wasp.Project.Common (CompileError, CompileWarning, WaspProjectDir)
import Wasp.Project.Db (makeDevDatabaseUrl)
import Wasp.Project.Db.Migrations (findMigrationsDir)
import Wasp.Project.Deployment (loadUserDockerfileContents)
import Wasp.Project.Env (readDotEnvClient, readDotEnvServer)
import Wasp.Project.Vite (findCustomViteConfigPath)
import Wasp.Util (maybeToEither)
import qualified Wasp.Util.IO as IOUtil

analyzeWaspProject ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  IO (Either [CompileError] AS.AppSpec, [CompileWarning])
analyzeWaspProject waspDir options = do
  findWaspFile waspDir >>= \case
    Left e -> return (Left [e], [])
    Right waspFilePath -> do
      analyzeWaspFileContent waspFilePath >>= \case
        Left es -> return (Left es, [])
        Right declarations ->
          constructAppSpec waspDir options declarations

analyzeWaspFileContent :: Path' Abs File' -> IO (Either [CompileError] [AS.Decl])
analyzeWaspFileContent waspFilePath = do
  waspFileContent <- IOUtil.readFile waspFilePath
  let declsOrAnalyzeError = Analyzer.analyze waspFileContent
  return $
    Control.Arrow.left
      (map (showCompilerErrorForTerminal (waspFilePath, waspFileContent) . getErrorMessageAndCtx))
      declsOrAnalyzeError

constructAppSpec ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  [AS.Decl] ->
  IO (Either [CompileError] AS.AppSpec, [CompileWarning])
constructAppSpec waspDir options decls = do
  externalServerCodeFiles <-
    ExternalCode.readFiles (CompileOptions.externalServerCodeDirPath options)

  let externalClientCodeDirPath = CompileOptions.externalClientCodeDirPath options
  externalClientCodeFiles <- ExternalCode.readFiles externalClientCodeDirPath

  externalSharedCodeFiles <-
    ExternalCode.readFiles (CompileOptions.externalSharedCodeDirPath options)
  maybeMigrationsDir <- findMigrationsDir waspDir
  maybeUserDockerfileContents <- loadUserDockerfileContents waspDir
  configFiles <- CF.discoverConfigFiles waspDir G.CF.configFileRelocationMap
  let devDbUrl = makeDevDatabaseUrl waspDir decls
  serverEnvVars <- readDotEnvServer waspDir
  clientEnvVars <- readDotEnvClient waspDir

  let customViteConfigPath = findCustomViteConfigPath externalClientCodeFiles
  let appSpec =
        AS.AppSpec
          { AS.decls = decls,
            AS.waspProjectDir = waspDir,
            AS.externalClientFiles = externalClientCodeFiles,
            AS.externalServerFiles = externalServerCodeFiles,
            AS.externalSharedFiles = externalSharedCodeFiles,
            AS.migrationsDir = maybeMigrationsDir,
            AS.devEnvVarsServer = serverEnvVars,
            AS.devEnvVarsClient = clientEnvVars,
            AS.isBuild = CompileOptions.isBuild options,
            AS.userDockerfileContents = maybeUserDockerfileContents,
            AS.configFiles = configFiles,
            AS.devDatabaseUrl = devDbUrl,
            AS.customViteConfigPath = customViteConfigPath
          }
  let (validationErrors, validationWarnings) =
        let errsAndWarns = validateAppSpec appSpec
         in ( filter isValidationError errsAndWarns,
              filter isValidationWarning errsAndWarns
            )
  return
    ( if null validationErrors then Right appSpec else Left (show <$> validationErrors),
      show <$> validationWarnings
    )

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String (Path' Abs File'))
findWaspFile waspDir = do
  files <- fst <$> IOUtil.listDirectory waspDir
  return $ maybeToEither "Couldn't find a single *.wasp file." $ (waspDir </>) <$> find isWaspFile files
  where
    isWaspFile path =
      ".wasp"
        `isSuffixOf` toFilePath path
        && (length (toFilePath path) > length (".wasp" :: String))
