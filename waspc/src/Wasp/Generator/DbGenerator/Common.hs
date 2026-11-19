module Wasp.Generator.DbGenerator.Common
  ( dbMigrationsDirInDbRootDir,
    dbSchemaFileFromGeneratedAppComponentDir,
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
    findGeneratedPrismaClientSchemaFile,
  )
where

import StrongPath (Abs, Dir, File, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import qualified System.Directory as SD
import qualified System.FilePath as FP
import Wasp.Generator.Common (DbRootDir, GeneratedAppComponentDir, GeneratedAppDir)
import Wasp.Generator.Templates (TemplatesDir)
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

dbRootDirFromGeneratedAppComponentDir :: (GeneratedAppComponentDir d) => Path' (Rel d) (Dir DbRootDir)
dbRootDirFromGeneratedAppComponentDir = [reldir|../db|]

dbSchemaFileFromGeneratedAppComponentDir :: (GeneratedAppComponentDir d) => Path' (Rel d) (File PrismaDbSchema)
dbSchemaFileFromGeneratedAppComponentDir = dbRootDirFromGeneratedAppComponentDir </> dbSchemaFileInDbRootDir

dbSchemaFileInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (File PrismaDbSchema)
dbSchemaFileInGeneratedAppDir = dbRootDirInGeneratedAppDir </> dbSchemaFileInDbRootDir

-- | The schema that the Prisma client was last generated from. The Prisma CLI
-- writes the client into `.prisma/client` next to the installed
-- \@prisma/client package, and where that is depends on the package manager.
-- Wasp's link to the package leads there.
findGeneratedPrismaClientSchemaFile :: Path' Abs (Dir GeneratedAppDir) -> IO (Maybe (Path' Abs (File PrismaDbSchema)))
findGeneratedPrismaClientSchemaFile generatedAppDir = do
  isPrismaClientPackageLinked <- SD.doesDirectoryExist prismaClientPackageLink
  if isPrismaClientPackageLinked
    then do
      prismaClientPackageDir <- SD.canonicalizePath prismaClientPackageLink
      -- The package is `<node_modules>/@prisma/client`.
      let nodeModulesDir = FP.takeDirectory $ FP.takeDirectory prismaClientPackageDir
      Just <$> SP.parseAbsFile (nodeModulesDir FP.</> ".prisma" FP.</> "client" FP.</> "schema.prisma")
    else return Nothing
  where
    prismaClientPackageLink =
      FP.dropTrailingPathSeparator (SP.fromAbsDir generatedAppDir) FP.</> "node_modules" FP.</> "@prisma" FP.</> "client"

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
