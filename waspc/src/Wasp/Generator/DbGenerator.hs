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
import Data.Maybe (isNothing)
import StrongPath (Abs, Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import Wasp.CompileOptions (CompileOptions)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)
import qualified Wasp.Psl.Ast.Model as Psl.Ast.Model
import qualified Wasp.Psl.Generator.Model as Psl.Generator.Model
import Wasp.Wasp (Wasp, getMigrationsDir)
import qualified Wasp.Wasp as Wasp
import qualified Wasp.Wasp.Db as Wasp.Db
import Wasp.Wasp.Entity (Entity)
import qualified Wasp.Wasp.Entity as Wasp.Entity

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

genDb :: Wasp -> CompileOptions -> [FileDraft]
genDb wasp _ =
  [ genPrismaSchema wasp
  ]

preCleanup :: Wasp -> Path' Abs (Dir ProjectRootDir) -> CompileOptions -> IO ()
preCleanup wasp projectRootDir _ = do
  deleteGeneratedMigrationsDirIfRedundant wasp projectRootDir

deleteGeneratedMigrationsDirIfRedundant :: Wasp -> Path' Abs (Dir ProjectRootDir) -> IO ()
deleteGeneratedMigrationsDirIfRedundant wasp projectRootDir = do
  let waspMigrationsDirMissing = isNothing $ getMigrationsDir wasp
  projectMigrationsDirExists <- doesDirectoryExist projectMigrationsDirAbsFilePath
  when (waspMigrationsDirMissing && projectMigrationsDirExists) $ do
    putStrLn "A migrations directory does not exist in this Wasp root directory, but does in the generated project output directory."
    putStrLn $ "Deleting directory: " ++ projectMigrationsDirAbsFilePath ++ " ..."
    removeDirectoryRecursive projectMigrationsDirAbsFilePath
    putStrLn "Successfully deleted."
  where
    projectMigrationsDirAbsFilePath = SP.fromAbsDir $ projectRootDir </> dbRootDirInProjectRootDir </> dbMigrationsDirInDbRootDir

genPrismaSchema :: Wasp -> FileDraft
genPrismaSchema wasp = createTemplateFileDraft dstPath tmplSrcPath (Just templateData)
  where
    dstPath = dbSchemaFileInProjectRootDir
    tmplSrcPath = dbTemplatesDirInTemplatesDir </> dbSchemaFileInDbTemplatesDir

    templateData =
      object
        [ "modelSchemas" .= map entityToPslModelSchema (Wasp.getPSLEntities wasp),
          "datasourceProvider" .= (datasourceProvider :: String),
          "datasourceUrl" .= (datasourceUrl :: String)
        ]

    dbSystem = maybe Wasp.Db.SQLite Wasp.Db._system (Wasp.getDb wasp)
    (datasourceProvider, datasourceUrl) = case dbSystem of
      Wasp.Db.PostgreSQL -> ("postgresql", "env(\"DATABASE_URL\")")
      -- TODO: Report this error with some better mechanism, not `error`.
      Wasp.Db.SQLite ->
        if Wasp.getIsBuild wasp
          then error "SQLite (a default database) is not supported in production. To build your Wasp app for production, switch to a different database. Switching to PostgreSQL: https://wasp-lang.dev/docs/language/basic-elements/#migrating-from-sqlite-to-postgresql ."
          else ("sqlite", "\"file:./dev.db\"")

    entityToPslModelSchema :: Entity -> String
    entityToPslModelSchema entity =
      Psl.Generator.Model.generateModel $
        Psl.Ast.Model.Model (Wasp.Entity._name entity) (Wasp.Entity._pslModelBody entity)
