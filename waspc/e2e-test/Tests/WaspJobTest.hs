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
    waspCliNew,
  )
import Util ((<++>))
import Wasp.Project.Db (databaseUrlEnvVarName)

waspJob :: GoldenTest
waspJob = do
  let jobDecl =
        " job MySpecialJob { \n\
        \   executor: PgBoss, \n\
        \   perform: { \n\
        \     fn: import { foo } from \"@server/jobs/bar.js\"  \n\
        \   } \n\
        \ } \n"

  let jobFile =
        " export const foo = async (args) => { \n\
        \   return 1 \n\
        \ } \n"

  makeGoldenTest "waspJob" $
    sequence
      [ waspCliNew,
        cdIntoCurrentProject,
        setDbToPSQL,
        appendToWaspFile jobDecl,
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
