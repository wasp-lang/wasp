module Wasp.Project.Analyze
  ( analyzeWaspProject,
    analyzeWaspFileContent,
    findWaspFile,
    analyzePrismaSchema,
    WaspFile (..),
  )
where

import Control.Arrow (ArrowChoice (left))
import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import qualified Data.Aeson as Aeson
import Data.Conduit.Process.Typed (ExitCode (..))
import Data.List (find, isSuffixOf)
import StrongPath (Abs, Dir, File, File', Path', Rel, basename, castFile, fromAbsFile, relfile, toFilePath, (</>))
import qualified Wasp.Analyzer as Analyzer
import Wasp.Analyzer.AnalyzeError (getErrorMessageAndCtx)
import Wasp.Analyzer.Parser.Ctx (Ctx)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Decl.JSON ()
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.CompileOptions (CompileOptions)
import qualified Wasp.CompileOptions as CompileOptions
import qualified Wasp.ConfigFile as CF
import Wasp.Error (showCompilerErrorForTerminal)
import qualified Wasp.Generator.ConfigFile as G.CF
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common
  ( CompileError,
    CompileWarning,
    WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    findFileInWaspProjectDir,
    prismaSchemaFileInWaspProjectDir,
  )
import Wasp.Project.Db (makeDevDatabaseUrl)
import Wasp.Project.Db.Migrations (findMigrationsDir)
import Wasp.Project.Deployment (loadUserDockerfileContents)
import Wasp.Project.Env (readDotEnvClient, readDotEnvServer)
import qualified Wasp.Project.ExternalConfig as EC
import qualified Wasp.Project.ExternalFiles as ExternalFiles
import Wasp.Project.Vite (findCustomViteConfigPath)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Parser.Schema as Psl.Parser
import Wasp.Psl.Valid (getValidDbSystemFromPrismaSchema)
import qualified Wasp.Psl.Valid as PslV
import Wasp.Util.Aeson (encodeToString)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.StrongPath (replaceRelExtension)
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
              EC.analyzeExternalConfigs waspDir >>= \case
                Left errors -> return (Left errors, [])
                Right externalConfigs -> constructAppSpec waspDir options externalConfigs prismaSchemaAst declarations

data WaspFile
  = WaspLang !(Path' Abs (File WaspLangFile))
  | WaspTs !(Path' Abs (File WaspTsFile))

data WaspLangFile

data WaspTsFile

data CompiledWaspJsFile

data AppSpecDeclsJsonFile

analyzeWaspFile :: Path' Abs (Dir WaspProjectDir) -> Psl.Schema.Schema -> WaspFile -> IO (Either [CompileError] [AS.Decl])
analyzeWaspFile waspDir prismaSchemaAst = \case
  WaspLang waspFilePath -> analyzeWaspLangFile prismaSchemaAst waspFilePath
  WaspTs waspFilePath -> analyzeWaspTsFile waspDir prismaSchemaAst waspFilePath

analyzeWaspTsFile :: Path' Abs (Dir WaspProjectDir) -> Psl.Schema.Schema -> Path' Abs (File WaspTsFile) -> IO (Either [CompileError] [AS.Decl])
analyzeWaspTsFile waspProjectDir prismaSchemaAst waspFilePath = runExceptT $ do
  -- TODO: I'm not yet sure where tsconfig.node.json location should come from
  -- because we also need that knowledge when generating a TS SDK project.
  compiledWaspJsFile <- ExceptT $ compileWaspTsFile waspProjectDir [relfile|tsconfig.wasp.json|] waspFilePath
  declsJsonFile <- ExceptT $ executeMainWaspJsFile waspProjectDir prismaSchemaAst compiledWaspJsFile
  ExceptT $ readDecls prismaSchemaAst declsJsonFile

compileWaspTsFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  Path' Abs (File WaspTsFile) ->
  IO (Either [CompileError] (Path' Abs (File CompiledWaspJsFile)))
compileWaspTsFile waspProjectDir tsconfigNodeFileInWaspProjectDir waspFilePath = do
  chan <- newChan
  (_, tscExitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      ( runNodeCommandAsJob
          waspProjectDir
          "npx"
          [ "tsc",
            "-p",
            toFilePath (waspProjectDir </> tsconfigNodeFileInWaspProjectDir),
            "--noEmit",
            "false",
            "--outDir",
            toFilePath outDir
          ]
          J.Wasp
          chan
      )
  case tscExitCode of
    ExitFailure _status -> return $ Left ["Got TypeScript compiler errors for " ++ toFilePath waspFilePath ++ "."]
    ExitSuccess -> return $ Right absCompiledWaspJsFile
  where
    outDir = waspProjectDir </> dotWaspDirInWaspProjectDir
    absCompiledWaspJsFile = outDir </> compiledWaspJsFileInDotWaspDir
    compiledWaspJsFileInDotWaspDir = castFile $ case replaceRelExtension (basename waspFilePath) ".mjs" of
      Just path -> path
      Nothing -> error $ "Couldn't calculate the compiled JS file path for " ++ toFilePath waspFilePath ++ "."

executeMainWaspJsFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Psl.Schema.Schema ->
  Path' Abs (File CompiledWaspJsFile) ->
  IO (Either [CompileError] (Path' Abs (File AppSpecDeclsJsonFile)))
executeMainWaspJsFile waspProjectDir prismaSchemaAst absCompiledMainWaspJsFile = do
  chan <- newChan
  (_, runExitCode) <- do
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      ( runNodeCommandAsJob
          waspProjectDir
          "npx"
          -- TODO: Figure out how to keep running instructions in a single
          -- place (e.g., this is string the same as the package name, but it's
          -- repeated in two places).
          -- Before this, I had the entrypoint file hardcoded, which was bad
          -- too: waspProjectDir </> [relfile|node_modules/wasp-config/dist/run.js|]
          [ "wasp-config",
            fromAbsFile absCompiledMainWaspJsFile,
            fromAbsFile absDeclsOutputFile,
            encodeToString allowedEntityNames
          ]
          J.Wasp
          chan
      )
  case runExitCode of
    ExitFailure _status -> return $ Left ["Error while running the compiled *.wasp.mts file."]
    ExitSuccess -> return $ Right absDeclsOutputFile
  where
    absDeclsOutputFile = waspProjectDir </> dotWaspDirInWaspProjectDir </> [relfile|decls.json|]
    allowedEntityNames = Psl.Schema.getModelNames prismaSchemaAst

readDecls :: Psl.Schema.Schema -> Path' Abs (File AppSpecDeclsJsonFile) -> IO (Either [CompileError] [AS.Decl])
readDecls prismaSchemaAst declsJsonFile = runExceptT $ do
  entityDecls <- liftEither entityDeclsOrErrors
  remainingDecls <- ExceptT declsFromJsonOrError
  return $ entityDecls ++ remainingDecls
  where
    entityDeclsOrErrors =
      left (map fst) $
        left (map getErrorMessageAndCtx) $
          Analyzer.getEntityDecls prismaSchemaAst

    declsFromJsonOrError = do
      declsBytestring <- IOUtil.readFileBytes declsJsonFile
      return $ case Aeson.eitherDecode declsBytestring of
        Left err -> Left ["Error while parsing the declarations from JSON: " ++ err]
        Right value -> Right value

analyzeWaspLangFile :: Psl.Schema.Schema -> Path' Abs (File WaspLangFile) -> IO (Either [CompileError] [AS.Decl])
analyzeWaspLangFile prismaSchemaAst waspFilePath = do
  waspFileContent <- IOUtil.readFile waspFilePath
  left (map $ showCompilerErrorForTerminal (waspFilePath, waspFileContent))
    <$> analyzeWaspFileContent prismaSchemaAst waspFileContent

analyzeWaspFileContent :: Psl.Schema.Schema -> String -> IO (Either [(String, Ctx)] [AS.Decl])
analyzeWaspFileContent prismaSchemaAst = return . left (map getErrorMessageAndCtx) . Analyzer.analyze prismaSchemaAst

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
            AS.customViteConfigPath = customViteConfigPath
          }

  return $ runValidation ASV.validateAppSpec appSpec

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String WaspFile)
findWaspFile waspDir = do
  files <- fst <$> IOUtil.listDirectory waspDir
  return $ case (findWaspTsFile files, findWaspLangFile files) of
    (Just _, Just _) -> Left bothFilesFoundMessage
    (Nothing, Nothing) -> Left fileNotFoundMessage
    (Just waspTsFile, Nothing) -> Right waspTsFile
    (Nothing, Just waspLangFile) -> Right waspLangFile
  where
    findWaspTsFile files = WaspTs <$> findFileThatEndsWith ".wasp.mts" files
    findWaspLangFile files = WaspLang <$> findFileThatEndsWith ".wasp" files
    findFileThatEndsWith suffix files = castFile . (waspDir </>) <$> find ((suffix `isSuffixOf`) . toFilePath) files
    fileNotFoundMessage = "Couldn't find the *.wasp or a *.wasp.mts file in the " ++ toFilePath waspDir ++ " directory"
    bothFilesFoundMessage =
      "Found both *.wasp and *.wasp.mts files in the project directory. "
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
    couldntFindPrismaSchemaMessage = "Couldn't find the schema.prisma file in the " ++ toFilePath waspProjectDir ++ " directory. \nRead more: https://wasp-lang.dev/docs/migrate-from-0-13-to-0-14#migrate-to-the-new-schemaprisma-file"

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
