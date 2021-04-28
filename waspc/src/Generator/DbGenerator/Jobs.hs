module Generator.DbGenerator.Jobs
  ( migrateDev,
    runStudio,
  )
where

import Generator.Common (ProjectRootDir)
import Generator.DbGenerator (dbSchemaFileInProjectRootDir)
import qualified Generator.Job as J
import Generator.Job.Process (runNodeCommandAsJob)
import Generator.ServerGenerator.Common (serverRootDirInProjectRootDir)
import StrongPath (Abs, Dir, Path, (</>))
import qualified StrongPath as SP

migrateDev :: Path Abs (Dir ProjectRootDir) -> J.Job
migrateDev projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  -- NOTE(matija): We are running this command from server's root dir since that is where
  -- Prisma packages (cli and client) are currently installed.
  runNodeCommandAsJob
    serverDir
    "npx"
    [ "prisma",
      "migrate",
      "dev",
      "--schema",
      SP.toFilePath schemaFile
    ]
    J.Db

-- | Runs `prisma studio` - Prisma's db inspector.
runStudio :: Path Abs (Dir ProjectRootDir) -> J.Job
runStudio projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  runNodeCommandAsJob
    serverDir
    "npx"
    [ "prisma",
      "studio",
      "--schema",
      SP.toFilePath schemaFile
    ]
    J.Db
