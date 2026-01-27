module Tests.WaspDbResetTest (waspDbResetTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test (..), TestCase (..))
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import Wasp.Version (waspVersion)
import WaspProject.ShellCommands (appendToPrismaFile, createSeedFile, replaceMainWaspFile, waspCliCompile, waspCliDbMigrateDev, waspCliDbReset, waspCliDbSeed)

-- | We include a seeding script as part of the e2e test,
-- because Wasp skips the seeding during the reset (unlike Prisma).
waspDbResetTest :: Test
waspDbResetTest =
  Test
    "wasp-db-reset"
    [ TestCase
        "fail-outside-project"
        (return [waspCliDbResetFails]),
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
        "succeed-reset-database"
        ( sequence
            [ createTestWaspProject Minimal,
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
                  replaceMainWaspFile mainWaspWithSeeds,
                  waspCliDbSeed $ T.unpack seedScriptThatAssertsTasksTableIsEmptyName,
                  waspCliDbSeed $ T.unpack seedScriptThatPopulatesTasksTableName,
                  waspCliDbSeed $ T.unpack seedScriptThatAssertsTasksTableIsNotEmptyName,
                  waspCliDbReset,
                  waspCliDbSeed $ T.unpack seedScriptThatAssertsTasksTableIsEmptyName
                ]
            ]
        )
    ]
  where
    waspCliDbResetFails :: ShellCommand
    waspCliDbResetFails = "! wasp-cli db reset"

    taskPrismaModel :: T.Text
    taskPrismaModel =
      [trimming|
        model Task {
          id          Int     @id @default(autoincrement())
          description String
          isDone      Boolean @default(false)
        }
      |]

    mainWaspWithSeeds :: T.Text
    mainWaspWithSeeds =
      [trimming|
        app waspApp {
          wasp: {
            version: "^$textWaspVersion"
          },
          title: "wasp-app",
          head: [
            "<link rel='icon' href='/favicon.ico' />",
          ],
          db: {
            seeds: [
              import { $seedScriptThatPopulatesTasksTableName } from "@src/db/$seedScriptThatPopulatesTasksTableName",
              import { $seedScriptThatAssertsTasksTableIsEmptyName } from "@src/db/$seedScriptThatAssertsTasksTableIsEmptyName",
              import { $seedScriptThatAssertsTasksTableIsNotEmptyName } from "@src/db/$seedScriptThatAssertsTasksTableIsNotEmptyName"
            ]
          },
        }
        route RootRoute { path: "/", to: MainPage }
        page MainPage {
          component: import { MainPage } from "@src/MainPage"
        }
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

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion
