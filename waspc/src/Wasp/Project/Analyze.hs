{-# LANGUAGE TypeApplications #-}

module Wasp.Project.Analyze
  ( analyzeWaspProject,
    readPackageJsonFile,
    analyzeWaspFileContent,
    findWaspFile,
    findPackageJsonFile,
    analyzePrismaSchema,
  )
where

import Control.Arrow (ArrowChoice (left))
import qualified Data.Aeson as Aeson
import Data.List (find, isSuffixOf)
import StrongPath (Abs, Dir, File', Path', toFilePath, (</>))
import qualified Wasp.Analyzer as Analyzer
import Wasp.Analyzer.AnalyzeError (getErrorMessageAndCtx)
import Wasp.Analyzer.Parser.Ctx (Ctx)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Core.Decl as Decl
import Wasp.AppSpec.Entity (Entity, makeEntity)
import Wasp.AppSpec.PackageJson (PackageJson)
import Wasp.AppSpec.Valid (isValidationError, isValidationWarning, validateAppSpec)
import Wasp.CompileOptions (CompileOptions)
import qualified Wasp.CompileOptions as CompileOptions
import qualified Wasp.ConfigFile as CF
import Wasp.Error (showCompilerErrorForTerminal)
import qualified Wasp.Generator.ConfigFile as G.CF
import Wasp.Project.Common
  ( CompileError,
    CompileWarning,
    WaspProjectDir,
    findFileInWaspProjectDir,
    packageJsonInWaspProjectDir,
    prismaSchemaFileInWaspProjectDir,
  )
import Wasp.Project.Db (makeDevDatabaseUrl)
import Wasp.Project.Db.Migrations (findMigrationsDir)
import Wasp.Project.Deployment (loadUserDockerfileContents)
import Wasp.Project.Env (readDotEnvClient, readDotEnvServer)
import qualified Wasp.Project.ExternalFiles as ExternalFiles
import Wasp.Project.Vite (findCustomViteConfigPath)
import Wasp.Psl.Ast.Model (Model (Model), Schema (Schema))
import qualified Wasp.Psl.Ast.Model as Psl
import qualified Wasp.Psl.Ast.Model as Psl.Ast
import qualified Wasp.Psl.Parser.Model as Psl
import Wasp.Util (maybeToEither)
import qualified Wasp.Util.IO as IOUtil

analyzeWaspProject ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  IO (Either [CompileError] AS.AppSpec, [CompileWarning])
analyzeWaspProject waspDir options = do
  waspFilePathOrError <- maybeToEither [fileNotFoundMessage] <$> findWaspFile waspDir

  case waspFilePathOrError of
    Left err -> return (Left err, [])
    Right waspFilePath ->
      analyzePrismaSchema waspDir >>= \case
        Left errors -> return (Left errors, [])
        Right (parsedPrismaSchema, entities) ->
          analyzeWaspFile waspFilePath entities >>= \case
            Left errors -> return (Left errors, [])
            Right declarations ->
              analyzePackageJsonContent waspDir >>= \case
                Left errors -> return (Left errors, [])
                Right packageJsonContent -> constructAppSpec waspDir options packageJsonContent declarations entities parsedPrismaSchema
  where
    fileNotFoundMessage = "Couldn't find the *.wasp file in the " ++ toFilePath waspDir ++ " directory"

analyzeWaspFile :: Path' Abs File' -> [AS.Decl] -> IO (Either [CompileError] [AS.Decl])
analyzeWaspFile waspFilePath entities = do
  waspFileContent <- IOUtil.readFile waspFilePath
  left (map $ showCompilerErrorForTerminal (waspFilePath, waspFileContent))
    <$> analyzeWaspFileContent entities waspFileContent

analyzeWaspFileContent :: [AS.Decl] -> String -> IO (Either [(String, Ctx)] [AS.Decl])
analyzeWaspFileContent entities = return . left (map getErrorMessageAndCtx) . Analyzer.analyze entities

constructAppSpec ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  PackageJson ->
  [AS.Decl] ->
  [AS.Decl] ->
  Psl.Ast.Schema ->
  IO (Either [CompileError] AS.AppSpec, [CompileWarning])
constructAppSpec waspDir options packageJson decls entities parsedPrismaSchema = do
  externalCodeFiles <- ExternalFiles.readCodeFiles waspDir
  externalPublicFiles <- ExternalFiles.readPublicFiles waspDir
  customViteConfigPath <- findCustomViteConfigPath waspDir

  maybeMigrationsDir <- findMigrationsDir waspDir
  maybeUserDockerfileContents <- loadUserDockerfileContents waspDir
  configFiles <- CF.discoverConfigFiles waspDir G.CF.configFileRelocationMap
  let devDbUrl = makeDevDatabaseUrl waspDir decls
  serverEnvVars <- readDotEnvServer waspDir
  clientEnvVars <- readDotEnvClient waspDir

  let appSpec =
        AS.AppSpec
          { AS.decls = decls,
            AS.entities = entities,
            AS.prismaSchema = parsedPrismaSchema,
            AS.packageJson = packageJson,
            AS.waspProjectDir = waspDir,
            AS.externalCodeFiles = externalCodeFiles,
            AS.externalPublicFiles = externalPublicFiles,
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

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findWaspFile waspDir = do
  files <- fst <$> IOUtil.listDirectory waspDir
  return $ (waspDir </>) <$> find isWaspFile files
  where
    isWaspFile path =
      ".wasp"
        `isSuffixOf` toFilePath path
        && (length (toFilePath path) > length (".wasp" :: String))

analyzePackageJsonContent :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] PackageJson)
analyzePackageJsonContent waspProjectDir =
  findPackageJsonFile waspProjectDir >>= \case
    Just packageJsonFile -> readPackageJsonFile packageJsonFile
    Nothing -> return $ Left [fileNotFoundMessage]
  where
    fileNotFoundMessage = "couldn't find package.json file in the " ++ toFilePath waspProjectDir ++ " directory"

findPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir packageJsonInWaspProjectDir

readPackageJsonFile :: Path' Abs File' -> IO (Either [CompileError] PackageJson)
readPackageJsonFile packageJsonFile = do
  byteString <- IOUtil.readFileBytes packageJsonFile
  return $ maybeToEither ["Error parsing the package.json file"] $ Aeson.decode byteString

analyzePrismaSchema :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] (Psl.Ast.Schema, [AS.Decl]))
analyzePrismaSchema waspProjectDir = do
  prismaSchemaFile <- findPrismaSchemaFile waspProjectDir
  case prismaSchemaFile of
    Just pathToPrismaSchemaFile -> do
      prismaSchemaContent <- IOUtil.readFile pathToPrismaSchemaFile
      let prismaSchemaParseResult = Psl.parsePrismaSchema prismaSchemaContent
      case prismaSchemaParseResult of
        Left err -> return $ Left [err]
        Right parsedPrismaSchema -> do
          let entities = getEntitiesFromPrismaSchema parsedPrismaSchema
          return $ Right (parsedPrismaSchema, entities)
    Nothing -> return $ Left ["Couldn't find the Prisma schema file in the " ++ toFilePath waspProjectDir ++ " directory"]
  where
    getEntitiesFromPrismaSchema :: Psl.Ast.Schema -> [AS.Decl]
    getEntitiesFromPrismaSchema (Schema elements) =
      let models = [model | Psl.SchemaModel model <- elements]
       in parsedModelToDecl <$> models

    parsedModelToDecl :: Model -> AS.Decl
    parsedModelToDecl (Model name body) = Decl.makeDecl @Entity name $ makeEntity body

findPrismaSchemaFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findPrismaSchemaFile waspProjectDir = findFileInWaspProjectDir waspProjectDir prismaSchemaFileInWaspProjectDir
