module Wasp.Generator.DbGenerator.Common
  ( dbMigrationsDirInDbRootDir,
    dbSchemaFileFromGeneratedAppComponentRootDir,
    dbRootDirInGeneratedAppDir,
    dbSchemaChecksumOnLastDbConcurrenceFileInGeneratedAppDir,
    dbSchemaChecksumOnLastGenerateFileInGeneratedAppDir,
    dbSchemaFileInDbTemplatesDir,
    dbTemplatesDirInTemplatesDir,
    defaultMigrateArgs,
    getOnLastDbConcurrenceChecksumFileRefreshAction,
    MigrateArgs (..),
    ResetArgs (..),
    RefreshOnLastDbConcurrenceChecksumFile (..),
    DbSchemaChecksumOnLastDbConcurrenceFile,
    DbSchemaChecksumOnLastGenerateFile,
    PrismaDbSchema,
    dbSchemaFileInGeneratedAppDir,
    DbSchemaChecksumFile,
    dbSchemaFileInNodeModulesDir,
  )
where

import StrongPath (Dir, File, File', Path', Rel, reldir, relfile, (</>))
import Wasp.Generator.Common (DbRootDir, GeneratedAppComponentRootDir, GeneratedAppDir)
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.Project.Common (waspProjectDirFromGeneratedAppDir)
import Wasp.Project.Db.Migrations (DbMigrationsDir)

data DbTemplatesDir

-- | This file represents the Prisma db schema.
data PrismaDbSchema

class DbSchemaChecksumFile f

-- | This file represents the checksum of the Prisma db schema at the point
-- at which we last interacted with the DB to ensure they matched.
-- It is used to help warn the user of instances when they may need to migrate.
data DbSchemaChecksumOnLastDbConcurrenceFile

instance DbSchemaChecksumFile DbSchemaChecksumOnLastDbConcurrenceFile

-- | This file represents the checksum of the Prisma db schema
-- at the point at which `prisma generate` was last run. It is used
-- to know if we need to regenerate schema.prisma during web app generation or not.
data DbSchemaChecksumOnLastGenerateFile

instance DbSchemaChecksumFile DbSchemaChecksumOnLastGenerateFile

dbRootDirInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (Dir DbRootDir)
dbRootDirInGeneratedAppDir = [reldir|db|]

dbTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir DbTemplatesDir)
dbTemplatesDirInTemplatesDir = [reldir|db|]

dbSchemaFileInDbTemplatesDir :: Path' (Rel DbTemplatesDir) File'
dbSchemaFileInDbTemplatesDir = [relfile|schema.prisma|]

dbSchemaFileInDbRootDir :: Path' (Rel DbRootDir) (File PrismaDbSchema)
dbSchemaFileInDbRootDir = [relfile|schema.prisma|]

dbRootDirFromGeneratedAppComponentRootDir :: (GeneratedAppComponentRootDir d) => Path' (Rel d) (Dir DbRootDir)
dbRootDirFromGeneratedAppComponentRootDir = [reldir|../db|]

dbSchemaFileFromGeneratedAppComponentRootDir :: (GeneratedAppComponentRootDir d) => Path' (Rel d) (File PrismaDbSchema)
dbSchemaFileFromGeneratedAppComponentRootDir = dbRootDirFromGeneratedAppComponentRootDir </> dbSchemaFileInDbRootDir

dbSchemaFileInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (File PrismaDbSchema)
dbSchemaFileInGeneratedAppDir = dbRootDirInGeneratedAppDir </> dbSchemaFileInDbRootDir

dbSchemaFileInNodeModulesDir :: Path' (Rel GeneratedAppDir) (File PrismaDbSchema)
dbSchemaFileInNodeModulesDir = waspProjectDirFromGeneratedAppDir </> [relfile|node_modules/.prisma/client/schema.prisma|]

dbMigrationsDirInDbRootDir :: Path' (Rel DbRootDir) (Dir DbMigrationsDir)
dbMigrationsDirInDbRootDir = [reldir|migrations|]

dbSchemaChecksumOnLastDbConcurrenceFileInDbRootDir :: Path' (Rel DbRootDir) (File DbSchemaChecksumOnLastDbConcurrenceFile)
dbSchemaChecksumOnLastDbConcurrenceFileInDbRootDir = [relfile|schema.prisma.wasp-last-db-concurrence-checksum|]

dbSchemaChecksumOnLastDbConcurrenceFileInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (File DbSchemaChecksumOnLastDbConcurrenceFile)
dbSchemaChecksumOnLastDbConcurrenceFileInGeneratedAppDir = dbRootDirInGeneratedAppDir </> dbSchemaChecksumOnLastDbConcurrenceFileInDbRootDir

dbSchemaChecksumOnLastGenerateFileInDbRootDir :: Path' (Rel DbRootDir) (File DbSchemaChecksumOnLastGenerateFile)
dbSchemaChecksumOnLastGenerateFileInDbRootDir = [relfile|schema.prisma.wasp-generate-checksum|]

dbSchemaChecksumOnLastGenerateFileInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (File DbSchemaChecksumOnLastGenerateFile)
dbSchemaChecksumOnLastGenerateFileInGeneratedAppDir = dbRootDirInGeneratedAppDir </> dbSchemaChecksumOnLastGenerateFileInDbRootDir

data MigrateArgs = MigrateArgs
  { _migrationName :: Maybe String,
    _isCreateOnlyMigration :: Bool
  }
  deriving (Show, Eq)

defaultMigrateArgs :: MigrateArgs
defaultMigrateArgs = MigrateArgs {_migrationName = Nothing, _isCreateOnlyMigration = False}

data ResetArgs = ResetArgs
  { force :: Bool
  }

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
