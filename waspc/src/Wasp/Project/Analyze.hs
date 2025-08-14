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
    File,
    File',
    Path',
    Rel,
    fromAbsDir,
  )
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Decl.JSON ()
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.CompileOptions (CompileOptions)
import qualified Wasp.CompileOptions as CompileOptions
import qualified Wasp.ConfigFile as CF
import qualified Wasp.Generator.TailwindConfigFile as TCF
import Wasp.Project.Common
  ( CompileError,
    CompileWarning,
    SrcTsConfigFile,
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
analyzeWaspProject waspDir compileOptions = do
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
            Right declarations -> do
              let srcTsConfigPath = getSrcTsConfigInWaspProjectDir waspFilePath
              EC.readExternalConfigs waspDir srcTsConfigPath >>= \case
                Left externalConfigError -> return (Left [externalConfigError], [])
                Right externalConfigs ->
                  constructAppSpec
                    waspDir
                    compileOptions
                    externalConfigs
                    prismaSchemaAst
                    declarations
                    srcTsConfigPath

constructAppSpec ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  EC.ExternalConfigs ->
  Psl.Schema.Schema ->
  [AS.Decl] ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either [CompileError] AS.AppSpec, [CompileWarning])
constructAppSpec waspDir compileOptions externalConfigs parsedPrismaSchema decls srcTsConfigPath = do
  externalCodeFiles <- ExternalFiles.readCodeFiles waspDir
  externalPublicFiles <- ExternalFiles.readPublicFiles waspDir
  customViteConfigPath <- findCustomViteConfigPath waspDir

  maybeMigrationsDir <- findMigrationsDir waspDir
  maybeUserDockerfileContents <- loadUserDockerfileContents waspDir
  let dbSystem = getValidDbSystemFromPrismaSchema parsedPrismaSchema
  let devDbUrl = makeDevDatabaseUrl waspDir dbSystem decls
  serverEnvVars <- readDotEnvServer waspDir
  clientEnvVars <- readDotEnvClient waspDir
  tailwindConfigFilesRelocators <- CF.discoverConfigFiles waspDir TCF.tailwindConfigRelocationMap

  let appSpec =
        AS.AppSpec
          { AS.decls = decls,
            AS.prismaSchema = parsedPrismaSchema,
            AS.waspProjectDir = waspDir,
            AS.externalCodeFiles = externalCodeFiles,
            AS.externalPublicFiles = externalPublicFiles,
            AS.migrationsDir = maybeMigrationsDir,
            AS.devEnvVarsServer = serverEnvVars,
            AS.devEnvVarsClient = clientEnvVars,
            AS.isBuild = CompileOptions.isBuild compileOptions,
            AS.userDockerfileContents = maybeUserDockerfileContents,
            AS.devDatabaseUrl = devDbUrl,
            AS.customViteConfigPath = customViteConfigPath,
            AS.packageJson = EC._packageJson externalConfigs,
            AS.srcTsConfigPath = srcTsConfigPath,
            AS.srcTsConfig = EC._srcTsConfig externalConfigs,
            AS.tailwindConfigFilesRelocators = tailwindConfigFilesRelocators
          }

  return $ runValidation ASV.validateAppSpec appSpec

analyzePrismaSchema :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] Psl.Schema.Schema, [CompileWarning])
analyzePrismaSchema waspProjectDir = do
  findPrismaSchemaFile waspProjectDir >>= \case
    Just pathToPrismaSchemaFile -> do
      prismaSchemaContent <- IOUtil.readFile pathToPrismaSchemaFile

      case Psl.Parser.parsePrismaSchema prismaSchemaContent of
        Left err ->
          return (Left [couldntParsePrismaSchemaMessage ++ "\n\n" ++ err], [])
        Right parsedPrismaSchema -> return $ runValidation PslV.validatePrismaSchema parsedPrismaSchema
    Nothing -> return (Left [couldntFindPrismaSchemaMessage], [])
  where
    couldntParsePrismaSchemaMessage = "Wasp couldn't parse your schema.prisma file, please check if you have any errors in it."

    -- NOTE: linking here to migration docs because I think it's the most common reason why schema.prisma file is missing.
    -- After people mostly start using 0.14.0+ they will have schema.prisma file, so this message will be less relevant.
    -- If we see that this message is still relevant, we can change it to be more general.
    couldntFindPrismaSchemaMessage = "Couldn't find the schema.prisma file in the " ++ fromAbsDir waspProjectDir ++ " directory. \nRead more: https://wasp.sh/docs/migrate-from-0-13-to-0-14#migrate-to-the-new-schemaprisma-file"

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
