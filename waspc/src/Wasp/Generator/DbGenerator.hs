module Wasp.Generator.DbGenerator
  ( genDb,
    postWriteDbGeneratorActions,
    warnIfDbNeedsMigration,
    getEntitiesForPrismaSchema,
  )
where

import Data.Aeson (object, (.=))
import Data.List (find)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File, Path', Rel, (</>))
import Wasp.AppSpec (AppSpec, getEntities)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.AppSpec.Util (hasEntities)
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.DbGenerator.Common
  ( DbSchemaChecksumFile,
    DbSchemaChecksumOnLastDbConcurrenceFile,
    PrismaDbSchema,
    dbMigrationsDirInDbRootDir,
    dbRootDirInProjectRootDir,
    dbSchemaChecksumOnLastDbConcurrenceFileProjectRootDir,
    dbSchemaChecksumOnLastGenerateFileProjectRootDir,
    dbSchemaFileInDbTemplatesDir,
    dbSchemaFileInNodeModulesDir,
    dbSchemaFileInProjectRootDir,
    dbTemplatesDirInTemplatesDir,
  )
import qualified Wasp.Generator.DbGenerator.Operations as DbOps
import Wasp.Generator.FileDraft (FileDraft, createCopyDirFileDraft, createTemplateFileDraft)
import Wasp.Generator.FileDraft.CopyDirFileDraft (CopyDirFileDraftDstDirStrategy (RemoveExistingDstDir))
import Wasp.Generator.Monad
  ( Generator,
    GeneratorError (..),
    GeneratorWarning (GeneratorNeedsMigrationWarning),
    logAndThrowGeneratorError,
  )
import Wasp.Project.Db (validDbUrlExprForPrismaSchema)
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.Ast.ConfigBlock
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import qualified Wasp.Psl.Db as Pls.Db
import qualified Wasp.Psl.Format as Psl.Format
import qualified Wasp.Psl.Generator.Schema as Psl.Generator.Schema
import Wasp.Psl.Generator.WithCtx (generateWithCtx)
import Wasp.Util (checksumFromFilePath, hexToString, ifM, (<:>))
import qualified Wasp.Util.IO as IOUtil

genDb :: AppSpec -> Generator [FileDraft]
genDb spec =
  genPrismaSchema spec
    <:> (maybeToList <$> genMigrationsDir spec)

genPrismaSchema ::
  AppSpec ->
  Generator FileDraft
genPrismaSchema spec = do
  (datasourceProvider :: String) <- case dbSystem of
    AS.Db.PostgreSQL -> return Pls.Db.dbProviderPostgresqlStringLiteral
    AS.Db.SQLite ->
      if AS.isBuild spec
        then logAndThrowGeneratorError $ GenericGeneratorError "SQLite (a default database) is not supported in production. To build your Wasp app for production, switch to a different database. Switching to PostgreSQL: https://wasp.sh/docs/data-model/databases#migrating-from-sqlite-to-postgresql ."
        else return Pls.Db.dbProviderSqliteStringLiteral

  entities <- getEntitiesForPrismaSchema spec

  let templateData =
        object
          [ "modelSchemas" .= (entityToPslModelSchema <$> entities),
            "enumSchemas" .= enumSchemas,
            "viewSchemas" .= viewSchemas,
            "typeSchemas" .= typeSchemas,
            "datasourceSchema" .= generateConfigBlockSchema (getDatasource datasourceProvider),
            "generatorSchemas" .= (generateConfigBlockSchema <$> generators)
          ]

  return $ createTemplateFileDraft Wasp.Generator.DbGenerator.Common.dbSchemaFileInProjectRootDir tmplSrcPath (Just templateData)
  where
    tmplSrcPath = Wasp.Generator.DbGenerator.Common.dbTemplatesDirInTemplatesDir </> Wasp.Generator.DbGenerator.Common.dbSchemaFileInDbTemplatesDir
    dbSystem = ASV.getValidDbSystem spec

    enumSchemas = generateWithCtx (Psl.Generator.Schema.generateSchemaBlock . Psl.Schema.EnumBlock) <$> Psl.Schema.getEnums prismaSchemaAst

    viewSchemas = generateWithCtx (Psl.Generator.Schema.generateSchemaBlock . Psl.Schema.ViewBlock) <$> Psl.Schema.getViews prismaSchemaAst

    typeSchemas = generateWithCtx (Psl.Generator.Schema.generateSchemaBlock . Psl.Schema.TypeBlock) <$> Psl.Schema.getTypes prismaSchemaAst

    generateConfigBlockSchema = Psl.Generator.Schema.generateSchemaBlock . Psl.Schema.ConfigBlock

    getDatasource datasourceProvider =
      Psl.Ast.ConfigBlock.overrideKeyValuePairs
        [("provider", Psl.Argument.StringExpr datasourceProvider), ("url", validDbUrlExprForPrismaSchema)]
        -- We validated the Prisma schema so we know there is exactly one datasource block.
        (Psl.WithCtx.getNode $ head $ Psl.Schema.getDatasources prismaSchemaAst)

    generators =
      -- We are not overriding any values for now in the generator blocks.
      Psl.Ast.ConfigBlock.overrideKeyValuePairs [] . Psl.WithCtx.getNode <$> Psl.Schema.getGenerators prismaSchemaAst

    entityToPslModelSchema :: (String, AS.Entity.Entity) -> String
    entityToPslModelSchema (entityName, entity) =
      Psl.Generator.Schema.generateSchemaBlock $
        Psl.Schema.ModelBlock $
          Psl.Model.Model entityName (AS.Entity.getPslModelBody entity)

    prismaSchemaAst = AS.prismaSchema spec

-- | Returns a list of entities that should be included in the Prisma schema.
-- We put user defined entities as well as inject auth entities into the Prisma schema.
getEntitiesForPrismaSchema :: AppSpec -> Generator [(String, AS.Entity.Entity)]
getEntitiesForPrismaSchema spec = maybe (return userDefinedEntities) (DbAuth.injectAuth userDefinedEntities) maybeUserEntity
  where
    userDefinedEntities = getEntities spec

    maybeUserEntity :: Maybe (String, AS.Entity.Entity)
    maybeUserEntity = do
      auth <- AS.App.auth $ snd $ getApp spec
      let userEntityName = AS.refName . AS.Auth.userEntity $ auth
      find ((== userEntityName) . fst) userDefinedEntities

genMigrationsDir :: AppSpec -> Generator (Maybe FileDraft)
genMigrationsDir spec = return $ createCopyDirFileDraft RemoveExistingDstDir genProjectMigrationsDir <$> AS.migrationsDir spec
  where
    genProjectMigrationsDir = Wasp.Generator.DbGenerator.Common.dbRootDirInProjectRootDir </> Wasp.Generator.DbGenerator.Common.dbMigrationsDirInDbRootDir

-- | This function operates on generated code, and thus assumes the file drafts were written to disk
postWriteDbGeneratorActions :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO ([GeneratorWarning], [GeneratorError])
postWriteDbGeneratorActions spec dstDir = do
  formatPrismaSchemaFileOnDisk dstDir

  dbGeneratorWarnings <-
    -- It makes sense to check if db needs migration only if the db is known at this moment, for
    -- example if we are in development (`wasp start`).
    -- However if we are in build (`wasp build`), then there is no database to check against right
    -- now.
    if not (AS.isBuild spec)
      then maybeToList <$> warnIfDbNeedsMigration spec dstDir
      else pure []
  dbGeneratorErrors <- maybeToList <$> generatePrismaClient spec dstDir
  pure (dbGeneratorWarnings, dbGeneratorErrors)

-- | One of the checks we perform is to compare the Wasp generated schema.prisma file
-- and the schema.prisma file in the node_modules. Prisma formats the schema in node_modules
-- automatically, so we have to do the same to be able to compare them.
formatPrismaSchemaFileOnDisk :: Path' Abs (Dir ProjectRootDir) -> IO ()
formatPrismaSchemaFileOnDisk dstDir = do
  let generatedPrismaFilePath = dstDir </> Wasp.Generator.DbGenerator.Common.dbSchemaFileInProjectRootDir
  prismaFileContents <- IOUtil.readFileStrict generatedPrismaFilePath

  -- Ignoring Prisma schema errors here because we generated the schema ourselves and we know it is valid.
  formattedPrismaFileContents <- Psl.Format._formattedSchemaPsl <$> Psl.Format.prismaFormat prismaFileContents
  IOUtil.writeFile generatedPrismaFilePath $ T.unpack formattedPrismaFileContents

-- | Checks if user needs to run `wasp db migrate-dev` due to changes in schema.prisma, and if so, returns a warning.
-- When doing this, it looks at schema.prisma in the generated project.
--
-- This function makes following assumptions:
--  - schema.prisma will exist in the generated project even if no Entities were defined.
--    Due to how Prisma itself works, this assumption is currently fulfilled.
--  - schema.prisma.wasp-last-db-concurrence-checksum contains the checksum of the schema.prisma as it was when we last ensured it matched the DB.
--
-- Given that, there are two cases in which we wish to warn the user to run `wasp db migrate-dev`:
-- (1) If schema.prisma.wasp-last-db-concurrence-checksum exists, but is not equal to checksum(schema.prisma), we know there were changes to schema.prisma and they should migrate.
-- (2) If schema.prisma.wasp-last-db-concurrence-checksum does not exist, but the user has entities defined in schema.prisma (and thus, AppSpec).
--     This could imply they have never migrated locally, or that they have but are simply missing their generated project dir.
--     Common scenarios for the second warning include:
--       - After a fresh checkout, or after `wasp clean`.
--       - When they previously had no entities and just added their first.
--     In either of those scenarios, validate against DB itself to avoid redundant warnings.
--
--     NOTE: As one final optimization, if they do not have a schema.prisma.wasp-last-db-concurrence-checksum but the schema is
--     in sync with the database and all migrations are applied, we generate that file to avoid future checks.
warnIfDbNeedsMigration :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO (Maybe GeneratorWarning)
warnIfDbNeedsMigration spec projectRootDir = do
  dbSchemaChecksumFileExists <- IOUtil.doesFileExist dbSchemaChecksumFp
  if dbSchemaChecksumFileExists
    then warnIfSchemaDiffersFromChecksum dbSchemaFp dbSchemaChecksumFp
    else
      if entitiesExist
        then warnProjectDiffersFromDb projectRootDir
        else return Nothing
  where
    dbSchemaFp = projectRootDir </> Wasp.Generator.DbGenerator.Common.dbSchemaFileInProjectRootDir
    dbSchemaChecksumFp = projectRootDir </> Wasp.Generator.DbGenerator.Common.dbSchemaChecksumOnLastDbConcurrenceFileProjectRootDir
    entitiesExist = hasEntities spec

warnIfSchemaDiffersFromChecksum ::
  Path' Abs (File Wasp.Generator.DbGenerator.Common.PrismaDbSchema) ->
  Path' Abs (File Wasp.Generator.DbGenerator.Common.DbSchemaChecksumOnLastDbConcurrenceFile) ->
  IO (Maybe GeneratorWarning)
warnIfSchemaDiffersFromChecksum dbSchemaFileAbs dbschemachecksumfile =
  ifM
    (checksumFileMatchesSchema dbSchemaFileAbs dbschemachecksumfile)
    (return Nothing)
    ( return . Just $
        GeneratorNeedsMigrationWarning
          "Your Prisma schema has changed, please run `wasp db migrate-dev` when ready."
    )

-- | Checks if the project's Prisma schema file and migrations dir matches the DB state.
-- Issues a warning if it cannot connect, or if either check fails.
warnProjectDiffersFromDb :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe GeneratorWarning)
warnProjectDiffersFromDb projectRootDir = do
  schemaMatchesDb <- DbOps.doesSchemaMatchDb projectRootDir
  case schemaMatchesDb of
    Just True -> do
      allMigrationsAppliedToDb <- DbOps.areAllMigrationsAppliedToDb projectRootDir
      if allMigrationsAppliedToDb == Just True
        then do
          -- NOTE: Since we know schema == db and all migrations are applied,
          -- we can write this file to prevent future redundant Prisma checks.
          DbOps.writeDbSchemaChecksumToFile projectRootDir Wasp.Generator.DbGenerator.Common.dbSchemaChecksumOnLastDbConcurrenceFileProjectRootDir
          return Nothing
        else
          return . Just . GeneratorNeedsMigrationWarning $
            "You have unapplied migrations. Please run `wasp db migrate-dev` when ready."
    Just False ->
      return . Just . GeneratorNeedsMigrationWarning $
        "Your Prisma schema does not match your database, please run `wasp db migrate-dev`."
    -- NOTE: If there was an error, it could mean we could not connect to the SQLite db, since it
    -- does not exist. Or it could mean their databaseUrlEnvVar is wrong, or database is down, or
    -- any other number of causes. In any case, migrating will either solve it (in the SQLite case),
    -- or allow Prisma to give them enough info to troubleshoot.
    Nothing ->
      return . Just . GeneratorNeedsMigrationWarning $
        "Wasp was unable to verify your database is up to date."
          <> " Running `wasp db migrate-dev` may fix this and will provide more info."

generatePrismaClient :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO (Maybe GeneratorError)
generatePrismaClient spec projectRootDir = do
  isGeneratedPrismaClientValid <- and <$> sequence [isCurrentSchemaUpToDate, isNodeModulesSchemaSameAsProjectSchema]
  if not isGeneratedPrismaClientValid
    then generatePrismaClientIfEntitiesExist
    else return Nothing
  where
    isCurrentSchemaUpToDate :: IO Bool
    isCurrentSchemaUpToDate =
      checksumFileExistsAndMatchesSchema
        projectRootDir
        Wasp.Generator.DbGenerator.Common.dbSchemaFileInProjectRootDir

    -- If the generated client's schema doesn't match the current Wasp schema.prisma,
    -- we should regenerate the client.
    -- This can happen if the user runs `npx prisma generate` manually.
    isNodeModulesSchemaSameAsProjectSchema :: IO Bool
    isNodeModulesSchemaSameAsProjectSchema =
      checksumFileExistsAndMatchesSchema
        projectRootDir
        Wasp.Generator.DbGenerator.Common.dbSchemaFileInNodeModulesDir

    generatePrismaClientIfEntitiesExist :: IO (Maybe GeneratorError)
    generatePrismaClientIfEntitiesExist
      | entitiesExist =
          either (Just . GenericGeneratorError) (const Nothing) <$> DbOps.generatePrismaClient projectRootDir
      | otherwise = return Nothing

    entitiesExist = hasEntities spec

checksumFileExistsAndMatchesSchema ::
  Path' Abs (Dir ProjectRootDir) ->
  Path' (Rel ProjectRootDir) (File PrismaDbSchema) ->
  IO Bool
checksumFileExistsAndMatchesSchema projectRootDir schemaFileInProjectDir =
  ifM
    (IOUtil.doesFileExist checksumFileAbs)
    (checksumFileMatchesSchema dbSchemaFileAbs checksumFileAbs)
    (return False)
  where
    dbSchemaFileAbs = projectRootDir </> schemaFileInProjectDir
    checksumFileAbs = projectRootDir </> Wasp.Generator.DbGenerator.Common.dbSchemaChecksumOnLastGenerateFileProjectRootDir

checksumFileMatchesSchema :: (Wasp.Generator.DbGenerator.Common.DbSchemaChecksumFile f) => Path' Abs (File Wasp.Generator.DbGenerator.Common.PrismaDbSchema) -> Path' Abs (File f) -> IO Bool
checksumFileMatchesSchema dbSchemaFileAbs dbSchemaChecksumFileAbs =
  ifM
    (IOUtil.doesFileExist dbSchemaFileAbs)
    (schemaFileHasChecksum dbSchemaFileAbs)
    (return False)
  where
    schemaFileHasChecksum :: Path' Abs (File Wasp.Generator.DbGenerator.Common.PrismaDbSchema) -> IO Bool
    schemaFileHasChecksum schemaFile = do
      -- Read file strictly as the checksum may be later overwritten.
      dbChecksumFileContents <- IOUtil.readFileStrict dbSchemaChecksumFileAbs
      dbSchemaFileChecksum <- T.pack . hexToString <$> checksumFromFilePath schemaFile
      return $ dbSchemaFileChecksum == dbChecksumFileContents
