module Wasp.Generator.DbGenerator.Common
  ( dbMigrationsDirInDbRootDir,
    serverPrismaClientOutputDirEnv,
    webAppPrismaClientOutputDirEnv,
    prismaClientOutputDirInModuleDir,
    dbSchemaFileFromModuleDir,
    dbRootDirInProjectRootDir,
    dbSchemaChecksumOnLastDbConcurrenceFileProjectRootDir,
    dbSchemaChecksumOnLastGenerateFileProjectRootDir,
    dbSchemaFileInDbTemplatesDir,
    dbTemplatesDirInTemplatesDir,
    defaultMigrateArgs,
    getOnLastDbConcurrenceChecksumFileRefreshAction,
    MigrateArgs (..),
    RefreshOnLastDbConcurrenceChecksumFile (..),
    DbRootDir,
    DbSchemaChecksumOnLastDbConcurrenceFile,
    DbSchemaChecksumOnLastGenerateFile,
    PrismaDbSchema,
    serverRootDirFromDbRootDir,
    webAppRootDirFromDbRootDir,
    dbSchemaFileInProjectRootDir,
    prismaClientOutputDirEnvVar,
    databaseUrlEnvVar,
  )
where

import StrongPath (Dir, File, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.Common (DbMigrationsDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ServerGenerator.Common (ServerRootDir)
import Wasp.Generator.Templates (TemplatesDir)

data DbRootDir

data DbTemplatesDir

-- | This file represents the Prisma db schema (schema.prisma)
data PrismaDbSchema

-- | This file represents the checksum of schema.prisma at the point
-- at which we last interacted with the DB to ensure they matched.
-- It is used to help warn the user of instances when they may need to migrate.
data DbSchemaChecksumOnLastDbConcurrenceFile

-- | This file represents the checksum of schema.prisma
-- at the point at which `prisma generate` was last run. It is used
-- to know if we need to regenerate schema.prisma during web app generation or not.
data DbSchemaChecksumOnLastGenerateFile

serverRootDirFromDbRootDir :: Path' (Rel DbRootDir) (Dir ServerRootDir)
serverRootDirFromDbRootDir = [reldir|../server|]

webAppRootDirFromDbRootDir :: Path' (Rel DbRootDir) (Dir ServerRootDir)
webAppRootDirFromDbRootDir = [reldir|../web-app|]

dbRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir DbRootDir)
dbRootDirInProjectRootDir = [reldir|db|]

dbTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir DbTemplatesDir)
dbTemplatesDirInTemplatesDir = [reldir|db|]

dbSchemaFileInDbTemplatesDir :: Path' (Rel DbTemplatesDir) File'
dbSchemaFileInDbTemplatesDir = [relfile|schema.prisma|]

dbSchemaFileInDbRootDir :: Path' (Rel DbRootDir) (File PrismaDbSchema)
dbSchemaFileInDbRootDir = [relfile|schema.prisma|]

dbRootDirFromModuleDir :: Path' (Rel d) (Dir DbRootDir)
dbRootDirFromModuleDir = [reldir|../db|]

dbSchemaFileFromModuleDir :: Path' (Rel d) (File PrismaDbSchema)
dbSchemaFileFromModuleDir = dbRootDirFromModuleDir </> dbSchemaFileInDbRootDir

dbSchemaFileInProjectRootDir :: Path' (Rel ProjectRootDir) (File PrismaDbSchema)
dbSchemaFileInProjectRootDir = dbRootDirInProjectRootDir </> dbSchemaFileInDbRootDir

dbMigrationsDirInDbRootDir :: Path' (Rel DbRootDir) (Dir DbMigrationsDir)
dbMigrationsDirInDbRootDir = [reldir|migrations|]

dbSchemaChecksumOnLastDbConcurrenceFileInDbRootDir :: Path' (Rel DbRootDir) (File DbSchemaChecksumOnLastDbConcurrenceFile)
dbSchemaChecksumOnLastDbConcurrenceFileInDbRootDir = [relfile|schema.prisma.wasp-last-db-concurrence-checksum|]

dbSchemaChecksumOnLastDbConcurrenceFileProjectRootDir :: Path' (Rel ProjectRootDir) (File DbSchemaChecksumOnLastDbConcurrenceFile)
dbSchemaChecksumOnLastDbConcurrenceFileProjectRootDir = dbRootDirInProjectRootDir </> dbSchemaChecksumOnLastDbConcurrenceFileInDbRootDir

dbSchemaChecksumOnLastGenerateFileInDbRootDir :: Path' (Rel DbRootDir) (File DbSchemaChecksumOnLastGenerateFile)
dbSchemaChecksumOnLastGenerateFileInDbRootDir = [relfile|schema.prisma.wasp-generate-checksum|]

dbSchemaChecksumOnLastGenerateFileProjectRootDir :: Path' (Rel ProjectRootDir) (File DbSchemaChecksumOnLastGenerateFile)
dbSchemaChecksumOnLastGenerateFileProjectRootDir = dbRootDirInProjectRootDir </> dbSchemaChecksumOnLastGenerateFileInDbRootDir

prismaClientOutputDirEnvVar :: String
prismaClientOutputDirEnvVar = "PRISMA_CLIENT_OUTPUT_DIR"

databaseUrlEnvVar :: String
databaseUrlEnvVar = "DATABASE_URL"

prismaClientOutputDirInModuleDir :: Path' (Rel d) (Dir ServerRootDir)
prismaClientOutputDirInModuleDir = [reldir|node_modules/.prisma/client|]

serverPrismaClientOutputDirEnv :: (String, String)
serverPrismaClientOutputDirEnv = modulePrismaClientOutputDirEnv serverRootDirFromDbRootDir

webAppPrismaClientOutputDirEnv :: (String, String)
webAppPrismaClientOutputDirEnv = modulePrismaClientOutputDirEnv webAppRootDirFromDbRootDir

modulePrismaClientOutputDirEnv :: Path' (Rel DbRootDir) (Dir d) -> (String, String)
modulePrismaClientOutputDirEnv moduleRootDirFromDbRootDir =
  (prismaClientOutputDirEnvVar, SP.fromRelDir $ moduleRootDirFromDbRootDir </> prismaClientOutputDirInModuleDir)

data MigrateArgs = MigrateArgs
  { _migrationName :: Maybe String,
    _isCreateOnlyMigration :: Bool
  }
  deriving (Show, Eq)

defaultMigrateArgs :: MigrateArgs
defaultMigrateArgs = MigrateArgs {_migrationName = Nothing, _isCreateOnlyMigration = False}

-- | This type tells us what we need to do with the DbSchemaChecksumOnLastDbConcurrenceFile.
data RefreshOnLastDbConcurrenceChecksumFile
  = WriteOnLastDbConcurrenceChecksumFile
  | RemoveOnLastDbConcurrenceChecksumFile
  | IgnoreOnLastDbConcurrenceChecksumFile

getOnLastDbConcurrenceChecksumFileRefreshAction :: MigrateArgs -> RefreshOnLastDbConcurrenceChecksumFile
getOnLastDbConcurrenceChecksumFileRefreshAction migrateArgs =
  -- Since a create-only migration allows users to write any SQL, we remove the file to force
  -- revalidation with the DB. If it is a regular migration, we write it since they will be in sync.
  if _isCreateOnlyMigration migrateArgs
    then RemoveOnLastDbConcurrenceChecksumFile
    else WriteOnLastDbConcurrenceChecksumFile
