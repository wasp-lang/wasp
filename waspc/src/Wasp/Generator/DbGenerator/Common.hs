module Wasp.Generator.DbGenerator.Common
  ( dbMigrationsDirInDbRootDir,
    dbRootDirInProjectRootDir,
    dbSchemaChecksumOnLastDbConcurrenceFileProjectRootDir,
    dbSchemaChecksumOnLastGenerateFileProjectRootDir,
    dbSchemaFileInDbTemplatesDir,
    dbSchemaFileInProjectRootDir,
    dbTemplatesDirInTemplatesDir,
    parseMigrateArgs,
    emptyMigrateArgs,
    asArgs,
    getOnLastDbConcurrenceChecksumAction,
    MigrateArgs (..),
    OnLastDbConcurrenceChecksumAction (..),
  )
where

import StrongPath (Dir, File, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.Common (DbMigrationsDir)
import Wasp.Generator.Common (ProjectRootDir)
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

dbRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir DbRootDir)
dbRootDirInProjectRootDir = [reldir|db|]

dbTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir DbTemplatesDir)
dbTemplatesDirInTemplatesDir = [reldir|db|]

dbSchemaFileInDbTemplatesDir :: Path' (Rel DbTemplatesDir) File'
dbSchemaFileInDbTemplatesDir = [relfile|schema.prisma|]

dbSchemaFileInDbRootDir :: Path' (Rel DbRootDir) File'
-- Generated schema file will be in the same relative location as the
-- template file within templates dir.
dbSchemaFileInDbRootDir = SP.castRel dbSchemaFileInDbTemplatesDir

dbSchemaFileInProjectRootDir :: Path' (Rel ProjectRootDir) File'
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

emptyMigrateArgs :: MigrateArgs
emptyMigrateArgs = MigrateArgs {_migrationName = Nothing, _isCreateOnlyMigration = False}

asArgs :: MigrateArgs -> [String]
asArgs migrateArgs = do
  concat . concat $
    [ [["--create-only"] | _isCreateOnlyMigration migrateArgs],
      [["--name", name] | Just name <- [_migrationName migrateArgs]]
    ]

parseMigrateArgs :: Maybe [String] -> MigrateArgs
parseMigrateArgs Nothing = emptyMigrateArgs
parseMigrateArgs (Just migrateArgs) = do
  go migrateArgs emptyMigrateArgs
  where
    go :: [String] -> MigrateArgs -> MigrateArgs
    go ("--create-only" : rest) mArgs = go rest $ mArgs {_isCreateOnlyMigration = True}
    go ("--name" : name : rest) mArgs = go rest $ mArgs {_migrationName = Just name}
    go _ mArgs = mArgs

data OnLastDbConcurrenceChecksumAction
  = WriteOnLastDbConcurrenceChecksum
  | RemoveOnLastDbConcurrenceChecksum
  | IgnoreOnLastDbConcurrenceChecksum

-- When we do a create-only migration, we need to remove the DbSchemaChecksumOnLastDbConcurrenceFile so they know to migrate.
-- When we migrate, we need to write it to indicate the local code and DB are in sync.
getOnLastDbConcurrenceChecksumAction :: MigrateArgs -> OnLastDbConcurrenceChecksumAction
getOnLastDbConcurrenceChecksumAction migrateArgs =
  if _isCreateOnlyMigration migrateArgs
    then RemoveOnLastDbConcurrenceChecksum
    else WriteOnLastDbConcurrenceChecksum
