module Wasp.Generator.DbGenerator.Jobs
  ( migrateDev,
    migrateDiff,
    generatePrismaClient,
    runStudio,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel, (</>))
import qualified StrongPath as SP
import qualified System.Info
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator.Common (dbSchemaFileInProjectRootDir)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInProjectRootDir)

-- | NOTE: The expectation is that `npm install` was already executed
-- such that we can use the locally installed package.
-- This assumption is ok since it happens during compilation now.
prismaInServerNodeModules :: Path' (Rel ProjectRootDir) File'
prismaInServerNodeModules = serverRootDirInProjectRootDir </> [SP.relfile|./node_modules/.bin/prisma|]

absPrismaExecutableFp :: Path' Abs (Dir ProjectRootDir) -> FilePath
absPrismaExecutableFp projectDir = SP.toFilePath $ projectDir </> prismaInServerNodeModules

migrateDev :: Path' Abs (Dir ProjectRootDir) -> Maybe String -> J.Job
migrateDev projectDir maybeMigrationName = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  let optionalMigrationArgs = maybe [] (\name -> ["--name", name]) maybeMigrationName

  -- NOTE(matija): We are running this command from server's root dir since that is where
  -- Prisma packages (cli and client) are currently installed.
  -- NOTE(martin): `prisma migrate dev` refuses to execute when interactivity is needed if stdout is being piped,
  --   because it assumes it is used in non-interactive environment. In our case we are piping both stdin and stdout
  --   so we do have interactivity, but Prisma doesn't know that.
  --   I opened an issue with Prisma https://github.com/prisma/prisma/issues/7113, but in the meantime
  --   we are using `script` to trick Prisma into thinking it is running in TTY (interactively).

  -- NOTE(martin): For this to work on Mac, filepath in the list below must be as it is now - not wrapped in any quotes.
  let prismaMigrateCmd = absPrismaExecutableFp projectDir : ["migrate", "dev", "--schema", SP.toFilePath schemaFile] ++ optionalMigrationArgs
  let scriptArgs =
        if System.Info.os == "darwin"
          then -- NOTE(martin): On MacOS, command that `script` should execute is treated as multiple arguments.
            ["-Fq", "/dev/null"] ++ prismaMigrateCmd
          else -- NOTE(martin): On Linux, command that `script` should execute is treated as one argument.
            ["-feqc", unwords prismaMigrateCmd, "/dev/null"]

  runNodeCommandAsJob serverDir "script" scriptArgs J.Db

-- | Diffs the Prisma schema file against the db.
-- Because of the --exit-code flag, it changes the exit code behavior
-- to signal if the diff is empty or not (Empty: 0, Error: 1, Not empty: 2)
migrateDiff :: Path' Abs (Dir ProjectRootDir) -> J.Job
migrateDiff projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir
  let prismaMigrateDiffCmdArgs =
        [ "migrate",
          "diff",
          "--from-schema-datamodel",
          SP.toFilePath schemaFile,
          "--to-schema-datasource",
          SP.toFilePath schemaFile,
          "--exit-code"
        ]

  runNodeCommandAsJob serverDir (absPrismaExecutableFp projectDir) prismaMigrateDiffCmdArgs J.Db

-- | Runs `prisma studio` - Prisma's db inspector.
runStudio :: Path' Abs (Dir ProjectRootDir) -> J.Job
runStudio projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir
  let prismaStudioCmdArgs = ["studio", "--schema", SP.toFilePath schemaFile]

  runNodeCommandAsJob serverDir (absPrismaExecutableFp projectDir) prismaStudioCmdArgs J.Db

generatePrismaClient :: Path' Abs (Dir ProjectRootDir) -> J.Job
generatePrismaClient projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir
  let prismaGenerateCmdArgs = ["generate", "--schema", SP.toFilePath schemaFile]

  runNodeCommandAsJob serverDir (absPrismaExecutableFp projectDir) prismaGenerateCmdArgs J.Db
