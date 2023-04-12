module Wasp.Project.Db.Migrations
  ( DbMigrationsDir,
    dbMigrationsDirInWaspProjectDir,
    findMigrationsDir,
  )
where

import StrongPath (Abs, Dir, Path', Rel, fromAbsDir, reldir, (</>))
import System.Directory (doesDirectoryExist)
import Wasp.Project.Common (WaspProjectDir)

data DbMigrationsDir

dbMigrationsDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir DbMigrationsDir)
dbMigrationsDirInWaspProjectDir = [reldir|migrations|]

findMigrationsDir ::
  Path' Abs (Dir WaspProjectDir) ->
  IO (Maybe (Path' Abs (Dir DbMigrationsDir)))
findMigrationsDir waspDir = do
  let migrationsAbsPath = waspDir </> dbMigrationsDirInWaspProjectDir
  migrationsExists <- doesDirectoryExist $ fromAbsDir migrationsAbsPath
  return $ if migrationsExists then Just migrationsAbsPath else Nothing
