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
import Data.Maybe (fromMaybe, isNothing)
import StrongPath (Abs, Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.CompileOptions (CompileOptions)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)
import qualified Wasp.Psl.Ast.Model as Psl.Ast.Model
import qualified Wasp.Psl.Generator.Model as Psl.Generator.Model

-- * Path definitions

data DbRootDir

data DbTemplatesDir

data DbMigrationsDir

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

-- * Db generator

genDb :: AppSpec -> [FileDraft]
genDb spec =
  [ genPrismaSchema spec
  ]

preCleanup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO ()
preCleanup spec projectRootDir = do
  deleteGeneratedMigrationsDirIfRedundant spec projectRootDir

deleteGeneratedMigrationsDirIfRedundant :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO ()
deleteGeneratedMigrationsDirIfRedundant spec projectRootDir = do
  let waspMigrationsDirMissing = isNothing $ AS.migrationDir spec
  projectMigrationsDirExists <- doesDirectoryExist projectMigrationsDirAbsFilePath
  when (waspMigrationsDirMissing && projectMigrationsDirExists) $ do
    putStrLn "A migrations directory does not exist in this Wasp root directory, but does in the generated project output directory."
    putStrLn $ "Deleting directory: " ++ projectMigrationsDirAbsFilePath ++ " ..."
    removeDirectoryRecursive projectMigrationsDirAbsFilePath
    putStrLn "Successfully deleted."
  where
    projectMigrationsDirAbsFilePath = SP.fromAbsDir $ projectRootDir </> dbRootDirInProjectRootDir </> dbMigrationsDirInDbRootDir

genPrismaSchema :: AppSpec -> FileDraft
genPrismaSchema spec = createTemplateFileDraft dstPath tmplSrcPath (Just templateData)
  where
    dstPath = dbSchemaFileInProjectRootDir
    tmplSrcPath = dbTemplatesDirInTemplatesDir </> dbSchemaFileInDbTemplatesDir

    templateData =
      object
        [ "modelSchemas" .= map entityToPslModelSchema (AS.getDecls @AS.Entity.Entity spec),
          "datasourceProvider" .= (datasourceProvider :: String),
          "datasourceUrl" .= (datasourceUrl :: String)
        ]

    dbSystem = fromMaybe AS.Db.SQLite (AS.Db.system =<< AS.App.db (snd $ AS.getApp spec))
    (datasourceProvider, datasourceUrl) = case dbSystem of
      AS.Db.PostgreSQL -> ("postgresql", "env(\"DATABASE_URL\")")
      -- TODO: Report this error with some better mechanism, not `error`.
      AS.Db.SQLite ->
        if AS.isBuild spec
          then error "SQLite (a default database) is not supported in production. To build your Wasp app for production, switch to a different database. Switching to PostgreSQL: https://wasp-lang.dev/docs/language/basic-elements/#migrating-from-sqlite-to-postgresql ."
          else ("sqlite", "\"file:./dev.db\"")

    entityToPslModelSchema :: (String, AS.Entity.Entity) -> String
    entityToPslModelSchema (entityName, entity) =
      Psl.Generator.Model.generateModel $
        Psl.Ast.Model.Model entityName (AS.Entity.getPslModelBody entity)
