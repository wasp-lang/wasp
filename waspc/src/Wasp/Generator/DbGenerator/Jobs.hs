{-# LANGUAGE TupleSections #-}

module Wasp.Generator.DbGenerator.Jobs
  ( migrateDev,
    migrateDiff,
    generatePrismaClient,
    runStudio,
    reset,
    seed,
    dbExecuteTest,
    migrateStatus,
    asPrismaCliArgs,
  )
where

import StrongPath (Abs, Dir, File', Path', (</>))
import qualified StrongPath as SP
import StrongPath.TH (relfile)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..), ResetArgs (..), dbSchemaFileInGeneratedAppDir)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInGeneratedAppDir)
import Wasp.Generator.ServerGenerator.Db.Seed (dbSeedNameEnvVarName)
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)
import Wasp.Project.Common (WaspProjectDir, waspProjectDirFromGeneratedAppDir)

migrateDev :: Path' Abs (Dir GeneratedAppDir) -> MigrateArgs -> J.Job
migrateDev generatedAppDir migrateArgs =
  -- NOTE(matija): We are running this command from server's root dir since that is where
  -- Prisma packages (cli and client) are currently installed.
  runPrismaCommandAsJobFromWaspServerDir
    generatedAppDir
    prismaArgs
  where
    schemaFile = generatedAppDir </> dbSchemaFileInGeneratedAppDir

    prismaArgs =
      [ "migrate",
        "dev",
        "--schema",
        SP.fromAbsFile schemaFile,
        "--skip-generate",
        -- NOTE(martin): We do "--skip-seed" here because I just think seeding happening automatically
        --   in some situations is too aggressive / confusing.
        "--skip-seed"
      ]
        ++ asPrismaCliArgs migrateArgs

asPrismaCliArgs :: MigrateArgs -> [String]
asPrismaCliArgs migrateArgs = do
  concat . concat $ [createOnlyArg, nameArg]
  where
    createOnlyArg =
      [["--create-only"] | _isCreateOnlyMigration migrateArgs]
    nameArg =
      [["--name", name] | Just name <- [_migrationName migrateArgs]]

-- | Diffs the Prisma schema file against the db.
-- Because of the --exit-code flag, it changes the exit code behavior
-- to signal if the diff is empty or not (Empty: 0, Error: 1, Not empty: 2)
migrateDiff :: Path' Abs (Dir GeneratedAppDir) -> J.Job
migrateDiff generatedAppDir =
  runPrismaCommandAsJobFromWaspServerDir
    generatedAppDir
    [ "migrate",
      "diff",
      "--from-schema-datamodel",
      SP.fromAbsFile schema,
      "--to-schema-datasource",
      SP.fromAbsFile schema,
      "--exit-code"
    ]
  where
    schema = generatedAppDir </> dbSchemaFileInGeneratedAppDir

-- | Checks to see if all migrations are applied to the DB.
-- An exit code of 0 means we successfully verified all migrations are applied.
-- An exit code of 1 could mean either: (a) there was a DB connection error,
-- or (b) there are pending migrations to apply.
-- Therefore, this should be checked **after** a command that ensures connectivity.
migrateStatus :: Path' Abs (Dir GeneratedAppDir) -> J.Job
migrateStatus generatedAppDir =
  runPrismaCommandAsJobFromWaspServerDir
    generatedAppDir
    ["migrate", "status", "--schema", SP.fromAbsFile schema]
  where
    schema = generatedAppDir </> dbSchemaFileInGeneratedAppDir

-- | Runs `prisma migrate reset`, which drops the tables (so schemas and data is lost) and then
-- reapplies all the migrations.
reset :: Path' Abs (Dir GeneratedAppDir) -> ResetArgs -> J.Job
reset generatedAppDir resetArgs =
  runPrismaCommandAsJobFromWaspServerDir
    generatedAppDir
    ( [ "migrate",
        "reset",
        "--schema",
        SP.fromAbsFile schema,
        "--skip-generate",
        -- NOTE(martin): We do "--skip-seed" here because I just think seeding
        --   happening automatically on reset is too aggressive / confusing.
        "--skip-seed"
      ]
        ++ if force resetArgs then ["--force"] else []
    )
  where
    schema = generatedAppDir </> dbSchemaFileInGeneratedAppDir

-- | Runs `prisma db seed`, which executes the seeding script specified in package.json in
--   prisma.seed field.
--   NOTE: We are running this command from server dir since that's where we defined the "prisma.seed"
--   script in package.json. In the future, we might want to allow users to specify the script name
--   in the project package.json, in which case we would run this command from project root dir.
seed :: Path' Abs (Dir GeneratedAppDir) -> String -> J.Job
-- NOTE: Since v 0.3, Prisma doesn't use --schema parameter for `db seed`.
seed generatedAppDir seedName =
  runPrismaCommandAsJobWithExtraEnv
    serverDir
    [(dbSeedNameEnvVarName, seedName)]
    generatedAppDir
    ["db", "seed"]
  where
    serverDir = generatedAppDir </> serverRootDirInGeneratedAppDir

-- | Checks if the DB is running and connectable by running
-- `prisma db execute --stdin --schema <path to db schema>`.
--  Runs the command in the generated server code directory so it has access to the database URL.
--
-- Since nothing is passed to stdin, `prisma db execute` just runs an empty
-- SQL command, which works perfectly for checking if the database is running.
dbExecuteTest :: Path' Abs (Dir GeneratedAppDir) -> J.Job
dbExecuteTest generatedAppDir =
  runPrismaCommandAsJobFromWaspServerDir generatedAppDir ["db", "execute", "--stdin", "--schema", SP.fromAbsFile schema]
  where
    schema = generatedAppDir </> dbSchemaFileInGeneratedAppDir

-- | Runs `prisma studio` - Prisma's db inspector.
runStudio :: Path' Abs (Dir GeneratedAppDir) -> J.Job
runStudio generatedAppDir =
  runPrismaCommandAsJobFromWaspServerDir generatedAppDir ["studio", "--schema", SP.fromAbsFile schema]
  where
    schema = generatedAppDir </> dbSchemaFileInGeneratedAppDir

generatePrismaClient :: Path' Abs (Dir GeneratedAppDir) -> J.Job
generatePrismaClient generatedAppDir =
  runPrismaCommandAsJobFromWaspServerDir
    generatedAppDir
    [ "generate",
      "--schema",
      SP.fromAbsFile schema,
      disablePrismaPromotionsFlag
    ]
  where
    schema = generatedAppDir </> dbSchemaFileInGeneratedAppDir

    -- Prisma CLI will output various promotions to the user, such as "hints" about new features.
    -- We want to disable these promotions because our users can't act on many of them (e.g. "Try out Prisma 6.0!").
    disablePrismaPromotionsFlag :: String
    disablePrismaPromotionsFlag = "--no-hints"

runPrismaCommandAsJobFromWaspServerDir :: Path' Abs (Dir GeneratedAppDir) -> [String] -> J.Job
runPrismaCommandAsJobFromWaspServerDir generatedAppDir cmdArgs =
  runPrismaCommandAsJobWithExtraEnv serverDir [] generatedAppDir cmdArgs
  where
    -- We must run our Prisma commands from the server dir for Prisma
    -- to pick up our .env file there like before.  In the future, we might want
    -- to reconsider how Prisma and the server env vars interact and change
    -- this. Text copied from: https://github.com/wasp-lang/wasp/pull/1662
    serverDir = generatedAppDir </> serverRootDirInGeneratedAppDir

runPrismaCommandAsJobWithExtraEnv ::
  Path' Abs (Dir a) ->
  [(String, String)] ->
  Path' Abs (Dir GeneratedAppDir) ->
  [String] ->
  J.Job
runPrismaCommandAsJobWithExtraEnv fromDir envVars generatedAppDir cmdArgs =
  runNodeCommandAsJobWithExtraEnv envVars fromDir (absPrismaExecutableFp waspProjectDir) cmdArgs J.Db
  where
    waspProjectDir = generatedAppDir </> waspProjectDirFromGeneratedAppDir

-- | NOTE: The expectation is that `npm install` was already executed
-- such that we can use the locally installed package.
-- This assumption is ok since it happens during compilation now.
absPrismaExecutableFp :: Path' Abs (Dir WaspProjectDir) -> FilePath
absPrismaExecutableFp waspProjectDir = SP.fromAbsFile prismaExecutableAbs
  where
    prismaExecutableAbs :: Path' Abs File'
    prismaExecutableAbs = waspProjectDir </> [relfile|./node_modules/.bin/prisma|]
