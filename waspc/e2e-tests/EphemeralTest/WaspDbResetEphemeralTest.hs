module EphemeralTest.WaspDbResetEphemeralTest (waspDbResetEphemeralTest) where

import Control.Monad.Reader (MonadReader (ask))
import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProjectFromMinimalStarter, withInEphemeralWaspProjectDir)
import WaspProject.ShellCommands (appendToPrismaFile, waspCliMigrate, WaspProjectContext (..))
import ShellCommands (createFile, ShellCommandBuilder, ShellCommand)
import StrongPath (parseRelDir, (</>))
import Data.Maybe (fromJust)

-- | We include a seeding script as part of the ephemeral test,
-- because our invaraint is to skip the seeding process during the reset.
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
            [ appendToPrismaFile taskPrismaModel,
              waspCliMigrate "foo",
              createSeedScript "populateTasksTable" seedScriptThatPopulatesTasksTable,
              createSeedScript "assertTasksTableIsEmpty" seedScriptThatAssertsTasksTableIsEmpty,
              createSeedScript "assertTasksTableIsNotEmpty" seedScriptThatAssertsTasksTableIsNotEmpty
            ]
        ),

      makeEphemeralTestCase
        "Assert the tasks table is initially empty" 
        (return "wasp-cli db seed assertTasksTableIsEmpty"),
      makeEphemeralTestCase
        "Setup: Add tasks to the tasks table"
        (return "wasp-cli db seed populateTasksTable"),
      makeEphemeralTestCase
        "Assert the database is no longer empty"
        (return "wasp-cli db seed assertTasksTableIsNotEmpty"),
      makeEphemeralTestCase
      -- db reset drops db/schema, creates new db/schema, applies migrations (skips seeding)
        "Should reset the database successfully"
        (return "wasp-cli db reset"),
      makeEphemeralTestCase
        "Assert the tasks table is empty"
        (return "wasp-cli db seed assertTasksTableIsEmpty")
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

    createSeedScript :: String -> String -> ShellCommandBuilder WaspProjectContext ShellCommand
    createSeedScript seedName seedContent = do
      waspProjectContext <- ask
      let seedDir = _waspProjectDir waspProjectContext </> fromJust (parseRelDir "./db/seeds/")
      createFile seedDir seedName seedContent

    seedScriptThatPopulatesTasksTable =
      unlines
        [ "import { prisma } from 'wasp/server'",
          "",
          "export async function seed() {",
          "  await prisma.task.create({",
          "    data: { description: 'Test task', isDone: false }",
          "  })",
          "}"
        ]

    seedScriptThatAssertsTasksTableIsEmpty =
      unlines
        [ "import { prisma } from 'wasp/server'",
          "",
          "export async function seed() {",
          "  const taskCount = await prisma.task.count()",
          "  if (taskCount !== 0) {",
          "    throw new Error(`Expected tasks table to be empty, but found ${taskCount} tasks`)",
          "  }",
          "}"
        ]

    seedScriptThatAssertsTasksTableIsNotEmpty =
      unlines
        [ "import { prisma } from 'wasp/server'",
          "",
          "export async function seed() {",
          "  const taskCount = await prisma.task.count()",
          "  if (taskCount === 0) {",
          "    throw new Error('Expected tasks table to have data, but it was empty')",
          "  }",
          "}"
        ]