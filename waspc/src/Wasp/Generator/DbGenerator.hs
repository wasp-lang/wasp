{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.DbGenerator
  ( genDb,
    warnIfDbSchemaChangedSinceLastMigration,
    genPrismaClient,
    postWriteDbGeneratorActions,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe, maybeToList)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Directory (doesFileExist)
import Wasp.AppSpec (AppSpec, getEntities)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator.Common
  ( dbMigrationsDirInDbRootDir,
    dbRootDirInProjectRootDir,
    dbSchemaChecksumOnLastGenerateFileProjectRootDir,
    dbSchemaChecksumOnLastMigrateFileProjectRootDir,
    dbSchemaFileInDbTemplatesDir,
    dbSchemaFileInProjectRootDir,
    dbTemplatesDirInTemplatesDir,
  )
import qualified Wasp.Generator.DbGenerator.Operations as DbOps
import Wasp.Generator.FileDraft (FileDraft, createCopyDirFileDraft, createTemplateFileDraft)
import Wasp.Generator.Monad
  ( Generator,
    GeneratorError (..),
    GeneratorWarning (GeneratorNeedsMigrationWarning),
    logAndThrowGeneratorError,
  )
import qualified Wasp.Psl.Ast.Model as Psl.Ast.Model
import qualified Wasp.Psl.Generator.Model as Psl.Generator.Model
import Wasp.Util (checksumFromFilePath, hexToString, ifM, (<:>))

genDb :: AppSpec -> Generator [FileDraft]
genDb spec =
  genPrismaSchema spec <:> (maybeToList <$> genMigrationsDir spec)

genPrismaSchema :: AppSpec -> Generator FileDraft
genPrismaSchema spec = do
  (datasourceProvider, datasourceUrl) <- case dbSystem of
    AS.Db.PostgreSQL -> return ("postgresql", "env(\"DATABASE_URL\")")
    AS.Db.SQLite ->
      if AS.isBuild spec
        then logAndThrowGeneratorError $ GenericGeneratorError "SQLite (a default database) is not supported in production. To build your Wasp app for production, switch to a different database. Switching to PostgreSQL: https://wasp-lang.dev/docs/language/features#migrating-from-sqlite-to-postgresql ."
        else return ("sqlite", "\"file:./dev.db\"")

  let templateData =
        object
          [ "modelSchemas" .= map entityToPslModelSchema (AS.getDecls @AS.Entity.Entity spec),
            "datasourceProvider" .= (datasourceProvider :: String),
            "datasourceUrl" .= (datasourceUrl :: String),
            "isPassportRequired" .= maybe False AS.App.Auth.passportRequired (AS.App.auth (snd $ getApp spec))
          ]

  return $ createTemplateFileDraft dstPath tmplSrcPath (Just templateData)
  where
    dstPath = dbSchemaFileInProjectRootDir
    tmplSrcPath = dbTemplatesDirInTemplatesDir </> dbSchemaFileInDbTemplatesDir
    dbSystem = fromMaybe AS.Db.SQLite (AS.Db.system =<< AS.App.db (snd $ getApp spec))

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

-- | This function operates on generated code, and thus assumes the file drafts were written to disk
postWriteDbGeneratorActions :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO ([GeneratorWarning], [GeneratorError])
postWriteDbGeneratorActions spec dstDir = do
  dbGeneratorWarnings <- maybeToList <$> warnIfDbSchemaChangedSinceLastMigration spec dstDir
  dbGeneratorErrors <- maybeToList <$> genPrismaClient spec dstDir
  return (dbGeneratorWarnings, dbGeneratorErrors)

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
    dbSchemaChecksumFp = SP.fromAbsFile $ projectRootDir </> dbSchemaChecksumOnLastMigrateFileProjectRootDir
    entitiesExist = not . null $ getEntities spec

    warnIf :: Bool -> String -> Maybe GeneratorWarning
    warnIf b msg = if b then Just $ GeneratorNeedsMigrationWarning msg else Nothing

genPrismaClient :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO (Maybe GeneratorError)
genPrismaClient spec projectRootDir = do
  ifM wasCurrentSchemaAlreadyGenerated (return Nothing) generatePrismaClientIfEntitiesExist
  where
    wasCurrentSchemaAlreadyGenerated :: IO Bool
    wasCurrentSchemaAlreadyGenerated = do
      let dbSchemaFp = SP.fromAbsFile $ projectRootDir SP.</> dbSchemaFileInProjectRootDir
      let dbSchemaChecksumFp = SP.fromAbsFile $ projectRootDir SP.</> dbSchemaChecksumOnLastGenerateFileProjectRootDir

      dbSchemaChecksumFileExists <- doesFileExist dbSchemaChecksumFp
      if dbSchemaChecksumFileExists
        then do
          dbSchemaFileChecksum <- hexToString <$> checksumFromFilePath dbSchemaFp
          dbChecksumFileContents <- readFile dbSchemaChecksumFp
          return $ dbSchemaFileChecksum == dbChecksumFileContents
        else return False

    generatePrismaClientIfEntitiesExist :: IO (Maybe GeneratorError)
    generatePrismaClientIfEntitiesExist = do
      let entitiesExist = not . null $ getEntities spec
      if entitiesExist
        then either (Just . GenericGeneratorError) (const Nothing) <$> DbOps.generatePrismaClient projectRootDir
        else return Nothing
