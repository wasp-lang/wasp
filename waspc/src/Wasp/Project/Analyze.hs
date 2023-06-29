module Wasp.Project.Analyze
  ( analyzeWaspProject,
  )
where

import Control.Arrow (ArrowChoice (left))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.List (find, isSuffixOf)
import StrongPath (Abs, Dir, File', Path', toFilePath, (</>))
import qualified Wasp.Analyzer as Analyzer
import Wasp.Analyzer.AnalyzeError (getErrorMessageAndCtx)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Valid (validateAppSpec)
import Wasp.CompileOptions (CompileOptions)
import qualified Wasp.CompileOptions as CompileOptions
import qualified Wasp.ConfigFile as CF
import Wasp.Error (showCompilerErrorForTerminal)
import qualified Wasp.ExternalCode as ExternalCode
import qualified Wasp.Generator.ConfigFile as G.CF
import Wasp.Project.Common (CompileError, WaspProjectDir)
import Wasp.Project.Db (makeDevDatabaseUrl)
import Wasp.Project.Db.Migrations (findMigrationsDir)
import Wasp.Project.Deployment (loadUserDockerfileContents)
import Wasp.Project.Env (readDotEnvClient, readDotEnvServer)
import Wasp.Util (maybeToEither)
import qualified Wasp.Util.IO as IOUtil

analyzeWaspProject ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  IO (Either [CompileError] AS.AppSpec)
analyzeWaspProject waspDir options = runExceptT $ do
  waspFilePath <- ExceptT $ Control.Arrow.left pure <$> findWaspFile waspDir
  declarations <- ExceptT $ analyzeWaspFileContent waspFilePath
  ExceptT $ constructAppSpec waspDir options declarations

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
  IO (Either [CompileError] AS.AppSpec)
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
            AS.devDatabaseUrl = devDbUrl
          }
  return $ case validateAppSpec appSpec of
    [] -> Right appSpec
    validationErrors -> Left $ map show validationErrors

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String (Path' Abs File'))
findWaspFile waspDir = do
  files <- fst <$> IOUtil.listDirectory waspDir
  return $ maybeToEither "Couldn't find a single *.wasp file." $ (waspDir </>) <$> find isWaspFile files
  where
    isWaspFile path =
      ".wasp"
        `isSuffixOf` toFilePath path
        && (length (toFilePath path) > length (".wasp" :: String))
