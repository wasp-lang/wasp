module Wasp.Common
  ( DbMigrationsDir,
    WaspProjectDir,
    dbMigrationsDirInWaspProjectDir,
  )
where

import StrongPath (Dir, Path', Rel, reldir)

data WaspProjectDir -- Root dir of Wasp project, containing source files.

data DbMigrationsDir

dbMigrationsDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir DbMigrationsDir)
dbMigrationsDirInWaspProjectDir = [reldir|migrations|]
