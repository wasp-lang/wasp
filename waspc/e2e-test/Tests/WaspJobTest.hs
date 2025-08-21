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
waspJob =
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
  where
    simpleJobDecl =
      unlines
        [ "job simpleJob {",
          "  executor: PgBoss,",
          "  perform: {",
          "    fn: import { foo } from \"@src/server/jobs/bar.js\"",
          "  },",
          "}"
        ]
    scheduledJobDecl =
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
    scheduledJobWithArgsDecl =
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
    jobFile =
      unlines
        [ "export const foo = async (args) => {",
          "  return 1",
          "}"
        ]

addServerEnvFile :: ShellCommandBuilder [ShellCommand]
addServerEnvFile = do
  sequence [createFile envFileContents "./" ".env.server"]
  where
    envFileContents =
      unlines
        [ databaseUrlEnvVarName <> "=" <> "mock-database-url"
        ]
