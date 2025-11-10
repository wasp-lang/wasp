module EphemeralTest.WaspDbResetEphemeralTest (waspDbResetEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProjectFromMinimalStarter, withInEphemeralWaspProjectDir)
import WaspProject.ShellCommands (appendToPrismaFile, waspCliMigrate, waspCliCompile, createSeedFile, replaceMainWaspFile, waspCliDbReset, waspCliDbSeed)
import WaspProject.FileSystem (seedsDirInWaspProjectDir, seedsFileInSeedsDir)
import StrongPath ((</>), fromRelFile)

-- | We include a seeding script as part of the ephemeral test,
-- because Wasp skips the seeding during the reset (unlike Prisma).
waspDbResetEphemeralTest :: EphemeralTest
waspDbResetEphemeralTest =
  makeEphemeralTest
    "wasp-db-reset"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        (return "! wasp-cli db reset"),
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        createEphemeralWaspProjectFromMinimalStarter,
      makeEphemeralTestCase
        "Setup: Add a Task model to prisma and migrate"
        ( withInEphemeralWaspProjectDir
            [ waspCliCompile,
              appendToPrismaFile taskPrismaModel,
              waspCliMigrate "foo",
              createSeedFile
                seedScriptThatPopulatesTasksTable,
              createSeedFile
                seedScriptThatAssertsTasksTableIsEmpty,
              createSeedFile
                seedScriptThatAssertsTasksTableIsNotEmpty,
              replaceMainWaspFile mainWaspWithSeeds
            ]
        ),
      makeEphemeralTestCase
        "Assert the tasks table is initially empty"
        -- FIXME: find a way without seed commands
        (withInEphemeralWaspProjectDir [waspCliDbSeed seedScriptThatAssertsTasksTableIsEmptyName]),
      makeEphemeralTestCase
        "Setup: Add tasks to the tasks table"
        -- FIXME: find a way without seed commands
        (withInEphemeralWaspProjectDir [waspCliDbSeed seedScriptThatPopulatesTasksTableName]),
      makeEphemeralTestCase
        "Assert the database is no longer empty"
        -- FIXME: find a way without seed commands
        (withInEphemeralWaspProjectDir [waspCliDbSeed seedScriptThatAssertsTasksTableIsNotEmptyName]),
      makeEphemeralTestCase
        "Should reset the database successfully"
        (withInEphemeralWaspProjectDir [waspCliDbReset]),
      makeEphemeralTestCase
        "Assert the tasks table is empty"
        -- FIXME: find a way without seed commands
        (withInEphemeralWaspProjectDir [waspCliDbSeed seedScriptThatAssertsTasksTableIsEmptyName])
    ]
  where
    taskPrismaModel =
      unlines
        [ "model Task {",
          "  id          Int     @id @default(autoincrement())",
          "  description String",
          "  isDone      Boolean @default(false)",
          "}"
        ]

    mainWaspWithSeeds =
      unlines
        [ "app waspApp {",
          "  wasp: {",
          "    version: \"^0.18.2\"",
          "  },",
          "  title: \"wasp-app\",",
          "  head: [",
          "    \"<link rel='icon' href='/favicon.ico' />\",",
          "  ],",
          "  db: {",
          "    seeds: [",
          "      import { " ++ seedScriptThatPopulatesTasksTableName ++ " } from \"@" ++ fromRelFile (seedsDirInWaspProjectDir </> seedsFileInSeedsDir) ++ "\",",
          "      import { " ++ seedScriptThatAssertsTasksTableIsEmptyName ++" } from \"@" ++ fromRelFile (seedsDirInWaspProjectDir </> seedsFileInSeedsDir) ++ "\",",
          "      import { " ++ seedScriptThatAssertsTasksTableIsNotEmptyName ++" } from \"@" ++ fromRelFile (seedsDirInWaspProjectDir </> seedsFileInSeedsDir) ++ "\"",
          "    ]",
          "  },",
          "}",
          "route RootRoute { path: \"/\", to: MainPage }",
          "page MainPage {",
          "  component: import { MainPage } from \"@src/MainPage\"",
          "}"
        ]

    seedScriptThatPopulatesTasksTableName = "populateTasks"
    seedScriptThatPopulatesTasksTable =
      unlines
        [ "import { prisma } from 'wasp/server'",
          "",
          "export async function " ++ seedScriptThatPopulatesTasksTableName ++ "() {",
          "  await prisma.task.create({",
          "    data: { description: 'Test task', isDone: false }",
          "  })",
          "}"
        ]

    seedScriptThatAssertsTasksTableIsEmptyName = "assertTasksEmpty"
    seedScriptThatAssertsTasksTableIsEmpty =
      unlines
        [ "import { prisma } from 'wasp/server'",
          "",
          "export async function " ++ seedScriptThatAssertsTasksTableIsEmptyName ++ "() {",
          "  const taskCount = await prisma.task.count()",
          "  if (taskCount !== 0) {",
          "    throw new Error(`Expected tasks table to be empty, but found ${taskCount} tasks`)",
          "  }",
          "}"
        ]

    seedScriptThatAssertsTasksTableIsNotEmptyName = "assertTasksNotEmpty"
    seedScriptThatAssertsTasksTableIsNotEmpty =
      unlines
        [ "import { prisma } from 'wasp/server'",
          "",
          "export async function " ++ seedScriptThatAssertsTasksTableIsNotEmptyName ++ "() {",
          "  const taskCount = await prisma.task.count()",
          "  if (taskCount === 0) {",
          "    throw new Error('Expected tasks table to have data, but it was empty')",
          "  }",
          "}"
        ]