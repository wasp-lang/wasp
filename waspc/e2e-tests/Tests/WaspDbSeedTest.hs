module Tests.WaspDbSeedTest (waspDbSeedTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    appendToPrismaFile,
    createSeedFile,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    waitUntil,
    waspCliCompile,
    waspCliDbMigrateDev,
    waspCliDbSeed,
    (~&&),
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Version (waspVersion)

waspDbSeedTest :: Test
waspDbSeedTest =
  Test
    "wasp-db-seed"
    [ TestCase
        "fail-outside-project"
        (return [waspCliDbSeedFails]),
      -- FIXME: find a way without seed commands
      -- Both in 'WaspDbResetTest.hs' and in `WaspDbSeedTest.hs`
      -- I have the following comments with `FIXME`.
      -- This is because I needed access database to either do some action or assert state.
      -- I didn't want to bring in any extra dependencies.
      -- I wanted it to be database agnostic.
      -- The only way I found to do it through Wasp was by using the seeding scripts.
      -- They can only return the exit code, but that is enough.
      -- An alternative would be to directly use the `npx prisma execute` from the server files,
      -- but I thought that typescript was more understandable than SQL (and more db agnostic).
      TestCase
        "succeed-seed-database"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  appendToPrismaFile taskPrismaModel,
                  waspCliDbMigrateDev "foo",
                  createSeedFile
                    (T.unpack seedScriptThatPopulatesTasksTableName <> ".ts")
                    seedScriptThatPopulatesTasksTable,
                  createSeedFile
                    (T.unpack seedScriptThatAssertsTasksTableIsEmptyName <> ".ts")
                    seedScriptThatAssertsTasksTableIsEmpty,
                  createSeedFile
                    (T.unpack seedScriptThatAssertsTasksTableIsNotEmptyName <> ".ts")
                    seedScriptThatAssertsTasksTableIsNotEmpty,
                  replaceMainWaspTsFile mainWaspTsWithSeeds,
                  waspCliDbSeed $ T.unpack seedScriptThatAssertsTasksTableIsEmptyName,
                  waspCliDbSeed $ T.unpack seedScriptThatPopulatesTasksTableName,
                  waspCliDbSeed $ T.unpack seedScriptThatAssertsTasksTableIsNotEmptyName
                ]
            ]
        ),
      TestCase
        "stop-seed-on-sigterm"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile taskPrismaModel,
                  createSeedFile
                    (T.unpack seedScriptThatWaitsName <> ".ts")
                    seedScriptThatWaits,
                  replaceMainWaspTsFile mainWaspTsWithSeedThatWaits,
                  startWaspDbSeedInBackground,
                  return $ waitUntil 300 ("[ -s " ++ seedPidFile ++ " ]") "The seed didn't start.",
                  -- We send SIGTERM only to Wasp, so that it's Wasp's job to
                  -- stop the seed.
                  return "kill -TERM \"$(cat .wasp/.projectlock)\"",
                  return $
                    waitUntil
                      60
                      "! kill -0 \"$WASP_DB_SEED_PID\" 2>/dev/null"
                      "Wasp didn't stop after SIGTERM.",
                  return $
                    waitUntil
                      30
                      ("! kill -0 \"$(cat " ++ seedPidFile ++ ")\" 2>/dev/null")
                      "The seed kept running after Wasp stopped."
                ]
            ]
        )
    ]
  where
    waspCliDbSeedFails :: ShellCommand
    waspCliDbSeedFails = "! $WASP_CLI_CMD db seed"

    taskPrismaModel :: T.Text
    taskPrismaModel =
      [trimming|
        model Task {
          id          Int     @id @default(autoincrement())
          description String
          isDone      Boolean @default(false)
        }
      |]

    mainWaspTsWithSeeds :: T.Text
    mainWaspTsWithSeeds =
      [trimming|
        import { app, page, route } from "@wasp.sh/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" };
        import { $seedScriptThatPopulatesTasksTableName } from "./src/db/$seedScriptThatPopulatesTasksTableName" with { type: "ref" };
        import { $seedScriptThatAssertsTasksTableIsEmptyName } from "./src/db/$seedScriptThatAssertsTasksTableIsEmptyName" with { type: "ref" };
        import { $seedScriptThatAssertsTasksTableIsNotEmptyName } from "./src/db/$seedScriptThatAssertsTasksTableIsNotEmptyName" with { type: "ref" };

        export default app({
          name: "waspDbSeedTest",
          title: "waspDbSeedTest",
          wasp: { version: "$textWaspVersion" },
          head: ["<link rel='icon' href='/favicon.ico' />"],
          db: {
            seeds: [
              $seedScriptThatPopulatesTasksTableName,
              $seedScriptThatAssertsTasksTableIsEmptyName,
              $seedScriptThatAssertsTasksTableIsNotEmptyName
            ]
          },
          spec: [
            route("RootRoute", "/", page(MainPage)),
          ]
        })
      |]

    seedScriptThatPopulatesTasksTableName :: T.Text
    seedScriptThatPopulatesTasksTableName = "populateTasks"
    seedScriptThatPopulatesTasksTable =
      [trimming|
        import { prisma } from 'wasp/server'

        export async function $seedScriptThatPopulatesTasksTableName() {
          await prisma.task.create({
            data: { description: 'Test task', isDone: false }
          })
        }
      |]

    seedScriptThatAssertsTasksTableIsEmptyName :: T.Text
    seedScriptThatAssertsTasksTableIsEmptyName = "assertTasksEmpty"
    seedScriptThatAssertsTasksTableIsEmpty =
      [trimming|
        import { prisma } from 'wasp/server'

        export async function $seedScriptThatAssertsTasksTableIsEmptyName() {
          const taskCount = await prisma.task.count()
          if (taskCount !== 0) {
            throw new Error(`Expected tasks table to be empty, but found $${taskCount} tasks`)
          }
        }
      |]

    seedScriptThatAssertsTasksTableIsNotEmptyName :: T.Text
    seedScriptThatAssertsTasksTableIsNotEmptyName = "assertTasksNotEmpty"
    seedScriptThatAssertsTasksTableIsNotEmpty =
      [trimming|
        import { prisma } from 'wasp/server'

        export async function $seedScriptThatAssertsTasksTableIsNotEmptyName() {
          const taskCount = await prisma.task.count()
          if (taskCount === 0) {
            throw new Error('Expected tasks table to have data, but it was empty')
          }
        }
      |]

    -- Stores the PID of the background process in `$WASP_DB_SEED_PID`.
    startWaspDbSeedInBackground :: ShellCommandBuilder WaspProjectContext ShellCommand
    startWaspDbSeedInBackground = do
      seedCommand <- waspCliDbSeed $ T.unpack seedScriptThatWaitsName
      return $
        ("{ SEED_PID_FILE=\"$PWD/" ++ seedPidFile ++ "\" " ++ seedCommand ++ " > wasp-db-seed.log 2>&1 & }")
          ~&& "WASP_DB_SEED_PID=$!"

    seedPidFile :: FilePath
    seedPidFile = "seed.pid"

    mainWaspTsWithSeedThatWaits :: T.Text
    mainWaspTsWithSeedThatWaits =
      [trimming|
        import { app, page, route } from "@wasp.sh/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" };
        import { $seedScriptThatWaitsName } from "./src/db/$seedScriptThatWaitsName" with { type: "ref" };

        export default app({
          name: "waspDbSeedTest",
          title: "waspDbSeedTest",
          wasp: { version: "$textWaspVersion" },
          head: ["<link rel='icon' href='/favicon.ico' />"],
          db: {
            seeds: [$seedScriptThatWaitsName]
          },
          spec: [
            route("RootRoute", "/", page(MainPage)),
          ]
        })
      |]

    -- Stores the PID of the seed's process in the file at `$SEED_PID_FILE`,
    -- and waits long enough for the test to stop it.
    seedScriptThatWaitsName :: T.Text
    seedScriptThatWaitsName = "waitUntilStopped"
    seedScriptThatWaits =
      [trimming|
        import { writeFileSync } from 'node:fs'

        export async function $seedScriptThatWaitsName() {
          writeFileSync(process.env.SEED_PID_FILE!, String(process.pid))
          await new Promise((resolve) => setTimeout(resolve, 10 * 60 * 1000))
        }
      |]

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion
