{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.DbGenerator
  ( genDb,
    preCleanup,
    dbRootDirInProjectRootDir,
    dbMigrationsDirInDbRootDir,
    dbSchemaFileInProjectRootDir,
  )
where

import Control.Monad (when)
import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import StrongPath (Abs, Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.Common (DbMigrationsDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createCopyDirFileDraft, createTemplateFileDraft)
import Wasp.Generator.Monad (Generator, GeneratorError (..), logAndThrowGeneratorError)
import Wasp.Generator.Templates (TemplatesDir)
import qualified Wasp.Psl.Ast.Model as Psl.Ast.Model
import qualified Wasp.Psl.Generator.Model as Psl.Generator.Model
import Wasp.Util ((<:>))

data DbRootDir

data DbTemplatesDir

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

preCleanup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO ()
preCleanup spec projectRootDir = do
  deleteGeneratedMigrationsDirIfRedundant spec projectRootDir

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
