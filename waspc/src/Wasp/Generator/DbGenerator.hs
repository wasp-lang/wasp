module Wasp.Generator.DbGenerator
  ( genDb,
    postWriteDbGeneratorActions,
    warnIfDbNeedsMigration,
    getEntitiesForPrismaSchema,
  )
where

import Data.Aeson (object, (.=))
import Data.List (find)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text, pack)
import StrongPath (Abs, Dir, File, Path', Rel, (</>))
import Wasp.AppSpec (AppSpec, getEntities)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.AppSpec.Valid (getApp)
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
import Wasp.Project.Db (databaseUrlEnvVarName)
import qualified Wasp.Psl.Ast.Model as Psl.Ast.Model
import qualified Wasp.Psl.Generator.Extensions as Psl.Generator.Extensions
import qualified Wasp.Psl.Generator.Model as Psl.Generator.Model
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
  (datasourceProvider :: String, datasourceUrl) <- case dbSystem of
    AS.Db.PostgreSQL -> return ("postgresql", makeEnvVarField databaseUrlEnvVarName)
    AS.Db.SQLite ->
      if AS.isBuild spec
        then logAndThrowGeneratorError $ GenericGeneratorError "SQLite (a default database) is not supported in production. To build your Wasp app for production, switch to a different database. Switching to PostgreSQL: https://wasp-lang.dev/docs/data-model/backends#migrating-from-sqlite-to-postgresql ."
        else return ("sqlite", "\"file:./dev.db\"")

  entities <- getEntitiesForPrismaSchema spec

  let templateData =
        object
          [ "modelSchemas" .= map entityToPslModelSchema entities,
            "datasourceProvider" .= datasourceProvider,
            "datasourceUrl" .= datasourceUrl,
            "prismaPreviewFeatures" .= prismaPreviewFeatures,
            "dbExtensions" .= dbExtensions
          ]

  return $ createTemplateFileDraft Wasp.Generator.DbGenerator.Common.dbSchemaFileInProjectRootDir tmplSrcPath (Just templateData)
  where
    tmplSrcPath = Wasp.Generator.DbGenerator.Common.dbTemplatesDirInTemplatesDir </> Wasp.Generator.DbGenerator.Common.dbSchemaFileInDbTemplatesDir
    dbSystem = fromMaybe AS.Db.SQLite $ AS.Db.system =<< AS.App.db (snd $ getApp spec)
    makeEnvVarField envVarName = "env(\"" ++ envVarName ++ "\")"
    prismaPreviewFeatures = show <$> (AS.Db.clientPreviewFeatures =<< AS.Db.prisma =<< AS.App.db (snd $ getApp spec))
    dbExtensions = Psl.Generator.Extensions.showDbExtensions <$> (AS.Db.dbExtensions =<< AS.Db.prisma =<< AS.App.db (snd $ getApp spec))

    entityToPslModelSchema :: (String, AS.Entity.Entity) -> String
    entityToPslModelSchema (entityName, entity) =
      Psl.Generator.Model.generateModel $
        Psl.Ast.Model.Model entityName (AS.Entity.getPslModelBody entity)

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
    entitiesExist = not . null $ getEntities spec

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
generatePrismaClient spec projectRootDir =
  ifM
    wasCurrentSchemaAlreadyGenerated
    (return Nothing)
    generatePrismaClientIfEntitiesExist
  where
    wasCurrentSchemaAlreadyGenerated :: IO Bool
    wasCurrentSchemaAlreadyGenerated =
      checksumFileExistsAndMatchesSchema
        projectRootDir
        Wasp.Generator.DbGenerator.Common.dbSchemaChecksumOnLastGenerateFileProjectRootDir

    generatePrismaClientIfEntitiesExist :: IO (Maybe GeneratorError)
    generatePrismaClientIfEntitiesExist
      | entitiesExist =
          either (Just . GenericGeneratorError) (const Nothing) <$> DbOps.generatePrismaClient projectRootDir
      | otherwise = return Nothing

    entitiesExist = not . null $ getEntities spec

checksumFileExistsAndMatchesSchema ::
  Wasp.Generator.DbGenerator.Common.DbSchemaChecksumFile f =>
  Path' Abs (Dir ProjectRootDir) ->
  Path' (Rel ProjectRootDir) (File f) ->
  IO Bool
checksumFileExistsAndMatchesSchema projectRootDir dbSchemaChecksumInProjectDir =
  ifM
    (IOUtil.doesFileExist checksumFileAbs)
    (checksumFileMatchesSchema dbSchemaFileAbs checksumFileAbs)
    (return False)
  where
    dbSchemaFileAbs = projectRootDir </> Wasp.Generator.DbGenerator.Common.dbSchemaFileInProjectRootDir
    checksumFileAbs = projectRootDir </> dbSchemaChecksumInProjectDir

checksumFileMatchesSchema :: Wasp.Generator.DbGenerator.Common.DbSchemaChecksumFile f => Path' Abs (File Wasp.Generator.DbGenerator.Common.PrismaDbSchema) -> Path' Abs (File f) -> IO Bool
checksumFileMatchesSchema dbSchemaFileAbs dbSchemaChecksumFileAbs = do
  -- Read file strictly as the checksum may be later overwritten.
  dbChecksumFileContents <- IOUtil.readFileStrict dbSchemaChecksumFileAbs
  schemaFileHasChecksum dbSchemaFileAbs dbChecksumFileContents
  where
    schemaFileHasChecksum :: Path' Abs (File Wasp.Generator.DbGenerator.Common.PrismaDbSchema) -> Text -> IO Bool
    schemaFileHasChecksum schemaFile checksum = do
      dbSchemaFileChecksum <- pack . hexToString <$> checksumFromFilePath schemaFile
      return $ dbSchemaFileChecksum == checksum
