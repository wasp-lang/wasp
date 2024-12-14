module Wasp.Project.Analyze
  ( analyzeWaspProject,
    analyzePrismaSchema,
    WaspFilePath (..),
  )
where

import Control.Arrow (ArrowChoice (left))
import StrongPath
  ( Abs,
    Dir,
    File',
    Path',
    fromAbsDir,
    fromAbsFile,
    fromRelFile,
    reldir,
    relfile,
    (</>),
  )
import qualified System.FilePath as FP
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Decl.JSON ()
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.CompileOptions (CompileOptions)
import qualified Wasp.CompileOptions as CompileOptions
import qualified Wasp.ConfigFile as CF
import qualified Wasp.Generator.ConfigFile as G.CF
import Wasp.Project.Common
  ( CompileError,
    CompileWarning,
    WaspFilePath (..),
    WaspProjectDir,
    findFileInWaspProjectDir,
    getSrcTsConfigInWaspProjectDir,
    prismaSchemaFileInWaspProjectDir,
  )
import Wasp.Project.Db (makeDevDatabaseUrl)
import Wasp.Project.Db.Migrations (findMigrationsDir)
import Wasp.Project.Deployment (loadUserDockerfileContents)
import Wasp.Project.Env (readDotEnvClient, readDotEnvServer)
import qualified Wasp.Project.ExternalConfig as EC
import qualified Wasp.Project.ExternalFiles as ExternalFiles
import Wasp.Project.Vite (findCustomViteConfigPath)
import Wasp.Project.WaspFile (analyzeWaspFile, findWaspFile)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Parser.Schema as Psl.Parser
import Wasp.Psl.Valid (getValidDbSystemFromPrismaSchema)
import qualified Wasp.Psl.Valid as PslV
import qualified Wasp.Util.IO as IOUtil
import Wasp.Valid (ValidationError)
import qualified Wasp.Valid as Valid

analyzeWaspProject ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  IO (Either [CompileError] AS.AppSpec, [CompileWarning])
analyzeWaspProject waspDir options = do
  waspFilePathOrError <- left (: []) <$> findWaspFile waspDir
  case waspFilePathOrError of
    Left err -> return (Left err, [])
    Right waspFilePath ->
      analyzePrismaSchema waspDir >>= \case
        (Left prismaSchemaErrors, prismaSchemaWarnings) -> return (Left prismaSchemaErrors, prismaSchemaWarnings)
        -- NOTE: we are ignoring prismaSchemaWarnings if the schema was parsed successfully
        (Right prismaSchemaAst, _) ->
          analyzeWaspFile waspDir prismaSchemaAst waspFilePath >>= \case
            Left errors -> return (Left errors, [])
            Right declarations ->
              EC.analyzeExternalConfigs waspDir (getSrcTsConfigInWaspProjectDir waspFilePath) >>= \case
                Left errors -> return (Left errors, [])
                Right declarations ->
                  EC.analyzeExternalConfigs waspDir (getSrcTsConfigInWaspProjectDir waspFilePath) >>= \case
                    Left errors -> return (Left errors, [])
                    Right externalConfigs -> constructAppSpec waspDir options externalConfigs prismaSchemaAst declarations

constructAppSpec ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  EC.ExternalConfigs ->
  Psl.Schema.Schema ->
  [AS.Decl] ->
  IO (Either [CompileError] AS.AppSpec, [CompileWarning])
constructAppSpec waspDir options externalConfigs parsedPrismaSchema decls = do
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

  let packageJsonContent = EC._packageJson externalConfigs
      srcTsConfigPath = EC._srcTsConfigPath externalConfigs

  let appSpec =
        AS.AppSpec
          { AS.decls = decls,
            AS.prismaSchema = parsedPrismaSchema,
            AS.packageJson = packageJsonContent,
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
            AS.customViteConfigPath = customViteConfigPath,
            AS.srcTsConfigPath = srcTsConfigPath
          }

  return $ runValidation ASV.validateAppSpec appSpec

waspDirExists :: Path' Abs (Dir WaspProjectDir) -> IO (Either String (Path' Abs (Dir WaspProjectDir)))
waspDirExists waspDir = do
  let waspDotWaspPath = waspDir </> [relfile|.wasp|]
  isFile <- IOUtil.doesFileExist waspDotWaspPath
  if isFile
    then return $ Left "The path to the Wasp project is a file, but it should be a directory."
    else return $ Right waspDir

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String WaspFilePath)
findWaspFile waspDir = do
  files <- fst <$> IOUtil.listDirectory waspDir
  return $ case (findWaspTsFile files, findWaspLangFile files) of
    (Just _, Just _) -> Left bothFilesFoundMessage
    (Nothing, Nothing) -> Left fileNotFoundMessage
    (Just waspTsFile, Nothing) -> Right waspTsFile
    (Nothing, Just waspLangFile) -> Right waspLangFile
  where
    findWaspTsFile files = WaspTs <$> findFileThatEndsWith ".wasp.ts" files
    findWaspLangFile files = WaspLang <$> findFileThatEndsWith ".wasp" files
    findFileThatEndsWith suffix files = castFile . (waspDir </>) <$> find ((suffix `isSuffixOf`) . fromRelFile) files
    fileNotFoundMessage = "Couldn't find the *.wasp or a *.wasp.ts file in the " ++ fromAbsDir waspDir ++ " directory"
    bothFilesFoundMessage =
      "Found both *.wasp and *.wasp.ts files in the project directory. "
        ++ "You must choose how you want to define your app (using Wasp or TypeScript) and only keep one of them."

analyzePrismaSchema :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] Psl.Schema.Schema, [CompileWarning])
analyzePrismaSchema waspProjectDir = do
  findPrismaSchemaFile waspProjectDir >>= \case
    Just pathToPrismaSchemaFile -> do
      prismaSchemaContent <- IOUtil.readFile pathToPrismaSchemaFile

      case Psl.Parser.parsePrismaSchema prismaSchemaContent of
        Left err ->
          return (Left [couldntParsePrismaSchemaMessage ++ "\n\n" ++ show err], [])
        Right parsedPrismaSchema -> return $ runValidation PslV.validatePrismaSchema parsedPrismaSchema
    Nothing -> return (Left [couldntFindPrismaSchemaMessage], [])
  where
    couldntParsePrismaSchemaMessage = "Wasp couldn't parse your schema.prisma file, please check if you have any errors in it."

    -- NOTE: linking here to migration docs because I think it's the most common reason why schema.prisma file is missing.
    -- After people mostly start using 0.14.0+ they will have schema.prisma file, so this message will be less relevant.
    -- If we see that this message is still relevant, we can change it to be more general.
    couldntFindPrismaSchemaMessage = "Couldn't find the schema.prisma file in the " ++ fromAbsDir waspProjectDir ++ " directory. \nRead more: https://wasp-lang.dev/docs/migrate-from-0-13-to-0-14#migrate-to-the-new-schemaprisma-file"

runValidation :: (result -> [ValidationError]) -> result -> (Either [CompileError] result, [CompileWarning])
runValidation getErrorsAndWarnings result =
  if null errors
    then (Right result, warnings)
    else (Left errors, warnings)
  where
    errors = showErrorsBy Valid.isValidationError errsAndWarns
    warnings = showErrorsBy Valid.isValidationWarning errsAndWarns
    errsAndWarns = getErrorsAndWarnings result
    showErrorsBy predicate = map show . filter predicate

findPrismaSchemaFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findPrismaSchemaFile waspProjectDir = findFileInWaspProjectDir waspProjectDir prismaSchemaFileInWaspProjectDir
