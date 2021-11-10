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
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import qualified System.Info

migrateDev :: Path' Abs (Dir ProjectRootDir) -> J.Job
migrateDev projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  -- NOTE(matija): We are running this command from server's root dir since that is where
  -- Prisma packages (cli and client) are currently installed.
  -- NOTE(martin): `prisma migrate dev` refuses to execute when interactivity is needed if stdout is being piped,
  --   because it assumes it is used in non-interactive environment. In our case we are piping both stdin and stdout
  --   so we do have interactivity, but Prisma doesn't know that.
  --   I opened an issue with Prisma https://github.com/prisma/prisma/issues/7113, but in the meantime
  --   we are using `script` to trick Prisma into thinking it is running in TTY (interactively).

  -- NOTE(martin): For this to work on Mac, filepath in the list below must be as it is now - not wrapped in any quotes.
  let npxPrismaCmd = ["npx", "prisma", "migrate", "dev", "--schema", SP.toFilePath schemaFile]
  let scriptArgs =
        if System.Info.os == "darwin"
          then -- NOTE(martin): On MacOS, command that `script` should execute is treated as multiple arguments.
            ["-Fq", "/dev/null"] ++ npxPrismaCmd
          else -- NOTE(martin): On Linux, command that `script` should execute is treated as one argument.
            ["-feqc", unwords npxPrismaCmd, "/dev/null"]

  runNodeCommandAsJob serverDir "script" scriptArgs J.Db

-- | Runs `prisma studio` - Prisma's db inspector.
runStudio :: Path' Abs (Dir ProjectRootDir) -> J.Job
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
