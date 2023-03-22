module Wasp.Generator.DbGenerator.Jobs
  ( migrateDev,
    migrateDiff,
    generatePrismaClient,
    runStudio,
    reset,
    migrateStatus,
    asPrismaCliArgs,
  )
where

import StrongPath (Abs, Dir, File, File', Path', (</>))
import qualified StrongPath as SP
import StrongPath.TH (relfile)
import qualified System.Info
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator.Common
  ( MigrateArgs (..),
    PrismaDbSchema,
    dbSchemaFileInProjectRootDir,
  )
import Wasp.Generator.Job (JobType)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInProjectRootDir)

migrateDev :: Path' Abs (Dir ProjectRootDir) -> MigrateArgs -> J.Job
migrateDev projectDir migrateArgs =
  -- NOTE(matija): We are running this command from server's root dir since that is where
  -- Prisma packages (cli and client) are currently installed.
  -- NOTE(martin): `prisma migrate dev` refuses to execute when interactivity is needed if stdout is being piped,
  --   because it assumes it is used in non-interactive environment. In our case we are piping both stdin and stdout
  --   so we do have interactivity, but Prisma doesn't know that.
  --   I opened an issue with Prisma https://github.com/prisma/prisma/issues/7113, but in the meantime
  --   we are using `script` to trick Prisma into thinking it is running in TTY (interactively).
  runNodeCommandAsJob serverDir "script" scriptArgs J.Db
  where
    serverDir = projectDir </> serverRootDirInProjectRootDir
    schemaFile = projectDir </> dbSchemaFileInProjectRootDir

    scriptArgs =
      if System.Info.os == "darwin"
        then -- NOTE(martin): On MacOS, command that `script` should execute is treated as multiple arguments.
          ["-Fq", "/dev/null"] ++ prismaMigrateCmd
        else -- NOTE(martin): On Linux, command that `script` should execute is treated as one argument.
          ["-feqc", unwords prismaMigrateCmd, "/dev/null"]

    -- NOTE(martin): For this to work on Mac, filepath in the list below must be as it is now - not
    -- wrapped in any quotes.
    prismaMigrateCmd =
      [ absPrismaExecutableFp projectDir,
        "migrate",
        "dev",
        "--schema",
        SP.fromAbsFile schemaFile,
        "--skip-generate"
      ]
        ++ asPrismaCliArgs migrateArgs

asPrismaCliArgs :: MigrateArgs -> [String]
asPrismaCliArgs migrateArgs = do
  concat . concat $
    [ [["--create-only"] | _isCreateOnlyMigration migrateArgs],
      [["--name", name] | Just name <- [_migrationName migrateArgs]]
    ]

-- | Diffs the Prisma schema file against the db.
-- Because of the --exit-code flag, it changes the exit code behavior
-- to signal if the diff is empty or not (Empty: 0, Error: 1, Not empty: 2)
migrateDiff :: Path' Abs (Dir ProjectRootDir) -> J.Job
migrateDiff projectDir = runPrismaCommandAsJob projectDir $ \schema ->
  [ "migrate",
    "diff",
    "--from-schema-datamodel",
    SP.fromAbsFile schema,
    "--to-schema-datasource",
    SP.fromAbsFile schema,
    "--exit-code"
  ]

-- | Checks to see if all migrations are applied to the DB.
-- An exit code of 0 means we successfully verified all migrations are applied.
-- An exit code of 1 could mean either: (a) there was a DB connection error,
-- or (b) there are pending migrations to apply.
-- Therefore, this should be checked **after** a command that ensures connectivity.
migrateStatus :: Path' Abs (Dir ProjectRootDir) -> J.Job
migrateStatus projectDir = runPrismaCommandAsJob projectDir $ \schema ->
  ["migrate", "status", "--schema", SP.fromAbsFile schema]

-- | Runs `prisma migrate reset`, which drops the tables (so schemas and data is lost) and then
-- reapplies all the migrations.
reset :: Path' Abs (Dir ProjectRootDir) -> J.Job
reset projectDir = runPrismaCommandAsJob projectDir $ \schema ->
  ["migrate", "reset", "--skip-generate", "--schema", SP.fromAbsFile schema]

-- | Runs `prisma studio` - Prisma's db inspector.
runStudio :: Path' Abs (Dir ProjectRootDir) -> J.Job
runStudio projectDir = runPrismaCommandAsJob projectDir $ \schema ->
  ["studio", "--schema", SP.fromAbsFile schema]

generatePrismaClient :: Path' Abs (Dir ProjectRootDir) -> (String, String) -> JobType -> J.Job
generatePrismaClient projectDir prismaClientOutputDirEnv jobType =
  runNodeCommandAsJobWithExtraEnv envVars serverRootDir prismaExecutable prismaGenerateCmdArgs jobType
  where
    envVars = [prismaClientOutputDirEnv]
    serverRootDir = projectDir </> serverRootDirInProjectRootDir
    prismaExecutable = absPrismaExecutableFp projectDir
    prismaGenerateCmdArgs = ["generate", "--schema", SP.fromAbsFile schemaFile]
    schemaFile = projectDir </> dbSchemaFileInProjectRootDir

runPrismaCommandAsJob ::
  Path' Abs (Dir ProjectRootDir) ->
  (Path' Abs (File PrismaDbSchema) -> [String]) ->
  J.Job
runPrismaCommandAsJob projectDir makeCmdArgs =
  runNodeCommandAsJob serverDir (absPrismaExecutableFp projectDir) (makeCmdArgs schemaFile) J.Db
  where
    serverDir = projectDir </> serverRootDirInProjectRootDir
    schemaFile = projectDir </> dbSchemaFileInProjectRootDir

-- | NOTE: The expectation is that `npm install` was already executed
-- such that we can use the locally installed package.
-- This assumption is ok since it happens during compilation now.
absPrismaExecutableFp :: Path' Abs (Dir ProjectRootDir) -> FilePath
absPrismaExecutableFp projectDir = SP.fromAbsFile prismaExecutableAbs
  where
    prismaExecutableAbs :: Path' Abs File'
    prismaExecutableAbs = projectDir </> serverRootDirInProjectRootDir </> [relfile|./node_modules/.bin/prisma|]
