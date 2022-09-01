module Wasp.Generator.DbGenerator.Common
  ( dbMigrationsDirInDbRootDir,
    dbRootDirInProjectRootDir,
    dbSchemaChecksumLastDbCheckFileProjectRootDir,
    dbSchemaChecksumOnLastGenerateFileProjectRootDir,
    dbSchemaFileInDbTemplatesDir,
    dbSchemaFileInProjectRootDir,
    dbTemplatesDirInTemplatesDir,
  )
where

import StrongPath (Dir, File, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.Common (DbMigrationsDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Templates (TemplatesDir)

data DbRootDir

data DbTemplatesDir

-- | This file represents the checksum of schema.prisma
-- at the point at which we last verified it matched the DB. It is used
-- to help warn the user of instances when they may need to migrate.
data DbSchemaChecksumLastDbCheckFile

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

dbSchemaChecksumLastDbCheckFileInDbRootDir :: Path' (Rel DbRootDir) (File DbSchemaChecksumLastDbCheckFile)
dbSchemaChecksumLastDbCheckFileInDbRootDir = [relfile|schema.prisma.wasp-last-db-check-checksum|]

dbSchemaChecksumLastDbCheckFileProjectRootDir :: Path' (Rel ProjectRootDir) (File DbSchemaChecksumLastDbCheckFile)
dbSchemaChecksumLastDbCheckFileProjectRootDir = dbRootDirInProjectRootDir </> dbSchemaChecksumLastDbCheckFileInDbRootDir

dbSchemaChecksumOnLastGenerateFileInDbRootDir :: Path' (Rel DbRootDir) (File DbSchemaChecksumOnLastGenerateFile)
dbSchemaChecksumOnLastGenerateFileInDbRootDir = [relfile|schema.prisma.wasp-generate-checksum|]

dbSchemaChecksumOnLastGenerateFileProjectRootDir :: Path' (Rel ProjectRootDir) (File DbSchemaChecksumOnLastGenerateFile)
dbSchemaChecksumOnLastGenerateFileProjectRootDir = dbRootDirInProjectRootDir </> dbSchemaChecksumOnLastGenerateFileInDbRootDir
