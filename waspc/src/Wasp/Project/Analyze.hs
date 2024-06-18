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
import Wasp.AppSpec.PackageJson (PackageJson)
import qualified Wasp.AppSpec.Valid as ASV
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
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Parser.Schema as Psl.Parser
import Wasp.Psl.Valid (getValidDbSystemFromPrismaSchema)
import qualified Wasp.Psl.Valid as PslV
import Wasp.Util (maybeToEither)
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Valid as Valid

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
        (Left prismaSchemaErrors, prismaSchemaWarnings) -> return (Left prismaSchemaErrors, prismaSchemaWarnings)
        -- NOTE: we are ignoring Prisma schema warnings if there are no errors for now
        (Right prismaSchemaAst, _prismaSchemaWarnings) ->
          analyzeWaspFile prismaSchemaAst waspFilePath >>= \case
            Left errors -> return (Left errors, [])
            Right declarations ->
              analyzePackageJsonContent waspDir >>= \case
                Left errors -> return (Left errors, [])
                Right packageJsonContent -> constructAppSpec waspDir options packageJsonContent prismaSchemaAst declarations
  where
    fileNotFoundMessage = "Couldn't find the *.wasp file in the " ++ toFilePath waspDir ++ " directory"

analyzeWaspFile :: Psl.Schema.Schema -> Path' Abs File' -> IO (Either [CompileError] [AS.Decl])
analyzeWaspFile prismaSchemaAst waspFilePath = do
  waspFileContent <- IOUtil.readFile waspFilePath
  left (map $ showCompilerErrorForTerminal (waspFilePath, waspFileContent))
    <$> analyzeWaspFileContent prismaSchemaAst waspFileContent

analyzeWaspFileContent :: Psl.Schema.Schema -> String -> IO (Either [(String, Ctx)] [AS.Decl])
analyzeWaspFileContent prismaSchemaAst = return . left (map getErrorMessageAndCtx) . Analyzer.analyze prismaSchemaAst

constructAppSpec ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  PackageJson ->
  Psl.Schema.Schema ->
  [AS.Decl] ->
  IO (Either [CompileError] AS.AppSpec, [CompileWarning])
constructAppSpec waspDir options packageJson parsedPrismaSchema decls = do
  externalCodeFiles <- ExternalFiles.readCodeFiles waspDir
  externalPublicFiles <- ExternalFiles.readPublicFiles waspDir
  customViteConfigPath <- findCustomViteConfigPath waspDir

  maybeMigrationsDir <- findMigrationsDir waspDir
  maybeUserDockerfileContents <- loadUserDockerfileContents waspDir
  configFiles <- CF.discoverConfigFiles waspDir G.CF.configFileRelocationMap
  let dbSystem = getValidDbSystemFromPrismaSchema parsedPrismaSchema
  let devDbUrl = makeDevDatabaseUrl waspDir dbSystem decls
  serverEnvVars <- readDotEnvServer waspDir
  clientEnvVars <- readDotEnvClient waspDir

  let appSpec =
        AS.AppSpec
          { AS.decls = decls,
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
        let errsAndWarns = ASV.validateAppSpec appSpec
         in ( filter Valid.isValidationError errsAndWarns,
              filter Valid.isValidationWarning errsAndWarns
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

analyzePrismaSchema :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] Psl.Schema.Schema, [CompileWarning])
analyzePrismaSchema waspProjectDir = do
  findPrismaSchemaFile waspProjectDir >>= \case
    Just pathToPrismaSchemaFile -> do
      prismaSchemaContent <- IOUtil.readFile pathToPrismaSchemaFile

      case Psl.Parser.parsePrismaSchema prismaSchemaContent of
        Left err ->
          return (Left [waspCouldntParsePrismaSchemaMessage ++ "\n\n" ++ show err], [])
        Right parsedPrismaSchema -> do
          let (validationErrors, validationWarnings) =
                let errsAndWarns = PslV.validatePrismaSchema parsedPrismaSchema
                 in ( filter Valid.isValidationError errsAndWarns,
                      filter Valid.isValidationWarning errsAndWarns
                    )
          return
            ( if null validationErrors then Right parsedPrismaSchema else Left (show <$> validationErrors),
              show <$> validationWarnings
            )
    Nothing -> return (Left ["Couldn't find the Prisma schema file in the " ++ toFilePath waspProjectDir ++ " directory"], [])
  where
    waspCouldntParsePrismaSchemaMessage = "Wasp couldn't parse your Prisma schema file, please check if you have any errors in it."

findPrismaSchemaFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findPrismaSchemaFile waspProjectDir = findFileInWaspProjectDir waspProjectDir prismaSchemaFileInWaspProjectDir
