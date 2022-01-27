{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.DbGenerator
  ( genDb,
    preCleanup,
    dbRootDirInProjectRootDir,
    dbMigrationsDirInDbRootDir,
    dbSchemaFileInProjectRootDir,
    writeDbSchemaChecksumToFile,
    checkGeneratedCode,
  )
where

import Control.Monad (when)
import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import StrongPath (Abs, Dir, File, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import Wasp.AppSpec (AppSpec, getEntities)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.Common (DbMigrationsDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createCopyDirFileDraft, createTemplateFileDraft)
import Wasp.Generator.Monad (Generator, GeneratorError (..), GeneratorWarning (GenericGeneratorWarning), logAndThrowGeneratorError)
import Wasp.Generator.Templates (TemplatesDir)
import qualified Wasp.Psl.Ast.Model as Psl.Ast.Model
import qualified Wasp.Psl.Generator.Model as Psl.Generator.Model
import Wasp.Util (checksumFromFilePath, hexToString, (<:>))

data DbRootDir

data DbTemplatesDir

-- | This file represents the checksum of schema.prisma
-- at the point at which db migrate-dev was last run. It is used
-- to help warn the user of instances they may need to migrate.
data DbSchemaChecksumFile

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

dbSchemaChecksumFileInDbRootDir :: Path' (Rel DbRootDir) (File DbSchemaChecksumFile)
dbSchemaChecksumFileInDbRootDir = [relfile|schema.prisma.wasp-checksum|]

dbSchemaChecksumFileInProjectRootDir :: Path' (Rel ProjectRootDir) (File DbSchemaChecksumFile)
dbSchemaChecksumFileInProjectRootDir = dbRootDirInProjectRootDir </> dbSchemaChecksumFileInDbRootDir

preCleanup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO ()
preCleanup = deleteGeneratedMigrationsDirIfRedundant

checkGeneratedCode :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO [GeneratorWarning]
checkGeneratedCode spec projectRootDir = maybeToList <$> warnIfDbSchemaChangedSinceLastMigration spec projectRootDir

genDb :: AppSpec -> Generator [FileDraft]
genDb spec =
  genPrismaSchema spec <:> (maybeToList <$> genMigrationsDir spec)

deleteGeneratedMigrationsDirIfRedundant :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO ()
deleteGeneratedMigrationsDirIfRedundant spec projectRootDir = do
  let waspMigrationsDirMissing = isNothing $ AS.migrationsDir spec
  projectMigrationsDirExists <- doesDirectoryExist projectMigrationsDirAbsFilePath
  when (waspMigrationsDirMissing && projectMigrationsDirExists) $ do
    putStrLn "A migrations directory does not exist in this Wasp root directory, but does in the generated project output directory."
    putStrLn $ "Deleting directory: " ++ projectMigrationsDirAbsFilePath ++ " ..."
    removeDirectoryRecursive projectMigrationsDirAbsFilePath
    putStrLn "Successfully deleted."
  where
    projectMigrationsDirAbsFilePath = SP.fromAbsDir $ projectRootDir </> dbRootDirInProjectRootDir </> dbMigrationsDirInDbRootDir

-- | Checks if user needs to run `wasp db migrate-dev` due to changes they did in schema.prisma, and if so, returns a warning.
-- When doing this, it looks at schema.prisma in the generated project.
--
-- This function makes following assumptions:
--  - schema.prisma will exist in the generated project even if no Entities were defined.
--    Due to how Prisma itself works, this assumption is currently fulfilled.
--  - schema.prisma.wasp-checksum contains the checksum of the schema.prisma as it was during the last `wasp db migrate-dev`.
--
-- Given that, there are two cases in which we wish to warn the user to run `wasp db migrate-dev`:
-- (1) If schema.prisma.wasp-checksum exists, but is not equal to checksum(schema.prisma), we know they made changes to schema.prisma and should migrate.
-- (2) If schema.prisma.wasp-checksum does not exist, but the user has entities defined in schema.prisma (and thus, AppSpec).
--     This could imply they have never migrated locally, or that they have but are simply missing their generated project dir.
--     Common scenarios for the second warning include:
--       - After a fresh checkout, or after `wasp clean`; possible false positives in these cases, but for safety, it's still preferable to warn.
--       - When they previously had no entities and just added their first.
warnIfDbSchemaChangedSinceLastMigration :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO (Maybe GeneratorWarning)
warnIfDbSchemaChangedSinceLastMigration spec projectRootDir = do
  dbSchemaChecksumFileExists <- doesFileExist dbSchemaChecksumFp

  if dbSchemaChecksumFileExists
    then do
      dbSchemaFileChecksum <- hexToString <$> checksumFromFilePath dbSchemaFp
      dbChecksumFileContents <- readFile dbSchemaChecksumFp
      return $ warnIf (dbSchemaFileChecksum /= dbChecksumFileContents) "Your Prisma schema has changed, you should run `wasp db migrate-dev`."
    else return $ warnIf entitiesExist "Please run `wasp db migrate-dev` to ensure the local project is fully initialized."
  where
    dbSchemaFp = SP.fromAbsFile $ projectRootDir </> dbSchemaFileInProjectRootDir
    dbSchemaChecksumFp = SP.fromAbsFile $ projectRootDir </> dbSchemaChecksumFileInProjectRootDir
    entitiesExist = not . null $ getEntities spec

    warnIf :: Bool -> String -> Maybe GeneratorWarning
    warnIf b msg = if b then Just $ GenericGeneratorWarning msg else Nothing

genPrismaSchema :: AppSpec -> Generator FileDraft
genPrismaSchema spec = do
  (datasourceProvider, datasourceUrl) <- case dbSystem of
    AS.Db.PostgreSQL -> return ("postgresql", "env(\"DATABASE_URL\")")
    AS.Db.SQLite ->
      if AS.isBuild spec
        then logAndThrowGeneratorError $ GenericGeneratorError "SQLite (a default database) is not supported in production. To build your Wasp app for production, switch to a different database. Switching to PostgreSQL: https://wasp-lang.dev/docs/language/features/#migrating-from-sqlite-to-postgresql ."
        else return ("sqlite", "\"file:./dev.db\"")

  let templateData =
        object
          [ "modelSchemas" .= map entityToPslModelSchema (AS.getDecls @AS.Entity.Entity spec),
            "datasourceProvider" .= (datasourceProvider :: String),
            "datasourceUrl" .= (datasourceUrl :: String)
          ]

  return $ createTemplateFileDraft dstPath tmplSrcPath (Just templateData)
  where
    dstPath = dbSchemaFileInProjectRootDir
    tmplSrcPath = dbTemplatesDirInTemplatesDir </> dbSchemaFileInDbTemplatesDir
    dbSystem = fromMaybe AS.Db.SQLite (AS.Db.system =<< AS.App.db (snd $ AS.getApp spec))

    entityToPslModelSchema :: (String, AS.Entity.Entity) -> String
    entityToPslModelSchema (entityName, entity) =
      Psl.Generator.Model.generateModel $
        Psl.Ast.Model.Model entityName (AS.Entity.getPslModelBody entity)

genMigrationsDir :: AppSpec -> Generator (Maybe FileDraft)
genMigrationsDir spec =
  return $
    AS.migrationsDir spec >>= \waspMigrationsDir ->
      Just $ createCopyDirFileDraft (SP.castDir genProjectMigrationsDir) (SP.castDir waspMigrationsDir)
  where
    genProjectMigrationsDir = dbRootDirInProjectRootDir </> dbMigrationsDirInDbRootDir

writeDbSchemaChecksumToFile :: Path' Abs (Dir ProjectRootDir) -> IO ()
writeDbSchemaChecksumToFile genProjectRootDir = do
  dbSchemaExists <- doesFileExist dbSchemaFp
  when dbSchemaExists $ do
    checksum <- hexToString <$> checksumFromFilePath dbSchemaFp
    writeFile dbSchemaChecksumFp checksum
  where
    dbSchemaFp = SP.fromAbsFile $ genProjectRootDir </> dbSchemaFileInProjectRootDir
    dbSchemaChecksumFp = SP.fromAbsFile $ genProjectRootDir </> dbSchemaChecksumFileInProjectRootDir
