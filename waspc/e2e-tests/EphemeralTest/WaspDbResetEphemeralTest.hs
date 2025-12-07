module EphemeralTest.WaspDbResetEphemeralTest (waspDbResetEphemeralTest) where

import qualified Data.Text as T
import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import NeatInterpolation (trimming)
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Wasp.Version (waspVersion)
import WaspProject.ShellCommands (appendToPrismaFile, createSeedFile, replaceMainWaspFile, waspCliCompile, waspCliDbMigrateDev, waspCliDbReset, waspCliDbSeed)

-- | We include a seeding script as part of the ephemeral test,
-- because Wasp skips the seeding during the reset (unlike Prisma).
waspDbResetEphemeralTest :: EphemeralTest
waspDbResetEphemeralTest =
  makeEphemeralTest
    "wasp-db-reset"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliDbResetFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Setup: Add a Task model to prisma and migrate"
        ( withInEphemeralWaspProjectDir
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
              replaceMainWaspFile mainWaspWithSeeds
            ]
        ),
      makeEphemeralTestCase
        "Assert the tasks table is initially empty"
        -- FIXME: find a way without seed commands
        (withInEphemeralWaspProjectDir [waspCliDbSeed $ T.unpack seedScriptThatAssertsTasksTableIsEmptyName]),
      makeEphemeralTestCase
        "Setup: Add tasks to the tasks table"
        -- FIXME: find a way without seed commands
        (withInEphemeralWaspProjectDir [waspCliDbSeed $ T.unpack seedScriptThatPopulatesTasksTableName]),
      makeEphemeralTestCase
        "Assert the database is no longer empty"
        -- FIXME: find a way without seed commands
        (withInEphemeralWaspProjectDir [waspCliDbSeed $ T.unpack seedScriptThatAssertsTasksTableIsNotEmptyName]),
      makeEphemeralTestCase
        "Should reset the database successfully"
        (withInEphemeralWaspProjectDir [waspCliDbReset]),
      makeEphemeralTestCase
        "Assert the tasks table is empty"
        -- FIXME: find a way without seed commands
        (withInEphemeralWaspProjectDir [waspCliDbSeed $ T.unpack seedScriptThatAssertsTasksTableIsEmptyName])
    ]
  where
    waspCliDbResetFails :: ShellCommandBuilder context ShellCommand
    waspCliDbResetFails = return "! wasp-cli db reset"

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
