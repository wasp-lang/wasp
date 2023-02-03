module Wasp.Generator.DbGenerator.Common
  ( dbMigrationsDirInDbRootDir,
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
    PrismaDbSchema,
    serverRootDirFromDbRootDir,
    webAppRootDirFromDbRootDir,
    dbSchemaFileInProjectRootDir,
  )
where

import StrongPath (Dir, File, File', Path', Rel, reldir, relfile, (</>))
import Wasp.Common (DbMigrationsDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ServerGenerator.Common (ServerRootDir)
import Wasp.Generator.Templates (TemplatesDir)

data DbRootDir

data DbTemplatesDir

-- | This file represents the checksum of schema.prisma at the point
-- at which we last interacted with the DB to ensure they matched.
-- It is used to help warn the user of instances when they may need to migrate.
data DbSchemaChecksumOnLastDbConcurrenceFile

-- | This file represents the checksum of schema.prisma
-- at the point at which `prisma generate` was last run. It is used
-- to know if we need to regenerate schema.prisma during web app generation or not.
data DbSchemaChecksumOnLastGenerateFile

data PrismaDbSchema

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
