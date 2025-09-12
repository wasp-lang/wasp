module Tests.WaspJobTest (waspJob) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    appendToWaspFile,
    cdIntoCurrentProject,
    createFile,
    setDbToPSQL,
    waspCliCompile,
    waspCliNewMinimalStarter,
  )
import Util ((<++>))
import Wasp.Project.Db (databaseUrlEnvVarName)

waspJob :: GoldenTest
waspJob = do
  let simpleJobDecl =
        unlines
          [ "job simpleJob {",
            "  executor: PgBoss,",
            "  perform: {",
            "    fn: import { foo } from \"@src/server/jobs/bar.js\"",
            "  },",
            "}"
          ]

  let scheduledJobDecl =
        unlines
          [ "job scheduleJob {",
            "  executor: PgBoss,",
            "  perform: {",
            "    fn: import { foo } from \"@src/server/jobs/bar.js\"",
            "  },",
            "  schedule: {",
            "    cron: \"0 * * * *\",",
            "    executorOptions: {",
            "      pgBoss: {=json { \"retryLimit\": 2 } json=}",
            "    }",
            "  }",
            "}"
          ]

  let scheduledJobWithArgsDecl =
        unlines
          [ "job scheduledJobWithArgs {",
            "  executor: PgBoss,",
            "  perform: {",
            "    fn: import { foo } from \"@src/server/jobs/bar.js\"",
            "  },",
            "  schedule: {",
            "    cron: \"0 * * * *\",",
            "    args: {=json { \"foo\": \"bar\" } json=},",
            "    executorOptions: {",
            "      pgBoss: {=json { \"retryLimit\": 2 } json=}",
            "    }",
            "  }",
            "}"
          ]

  let jobFile =
        unlines
          [ "export const foo = async (args) => {",
            "  return 1",
            "}"
          ]

  makeGoldenTest "waspJob" $
    sequence
      [ waspCliNewMinimalStarter,
        cdIntoCurrentProject,
        setDbToPSQL,
        appendToWaspFile simpleJobDecl,
        appendToWaspFile scheduledJobDecl,
        appendToWaspFile scheduledJobWithArgsDecl,
        createFile jobFile "./src/server/jobs" "bar.js"
      ]
      <++> addServerEnvFile
      <++> sequence
        [ waspCliCompile
        ]

addServerEnvFile :: ShellCommandBuilder [ShellCommand]
addServerEnvFile = do
  sequence [createFile envFileContents "./" ".env.server"]
  where
    envFileContents =
      unlines
        [ -- NOTE: Since we are using PSQL in this test, if we don't set custom
          -- database url in server/.env, Wasp will set its own, for managed dev db.
          -- That is problematic because Wasp's db url depends on project's abs path,
          -- which is not something we have constant during e2e tests, it depends
          -- on the location where the tests are being run.
          -- Therefore, we make sure to set custom database url here, to avoid .env
          -- changing between different machines / setups.
          databaseUrlEnvVarName <> "=" <> "mock-database-url"
        ]
