module Tests.WaspTaskTest (waspTaskTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    appendToFile,
    appendToPrismaFile,
    assertCommandOutputContainsAll,
    createFileInWaspProject,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    waspCliCompile,
    waspCliDbMigrateDev,
    waspCliTaskList,
    waspCliTaskRun,
  )
import StrongPath (relfile)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Version (waspVersion)

waspTaskTest :: Test
waspTaskTest =
  Test
    "wasp-task"
    [ TestCase
        "fail-outside-project"
        (return [waspCliTaskListOutsideProjectFails, waspCliTaskRunOutsideProjectFails]),
      TestCase
        "list-tasks"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile taskPrismaModel,
                  createFileInWaspProject [relfile|src/tasks.ts|] tasksFile,
                  replaceMainWaspTsFile mainWaspTs,
                  assertCommandOutputContainsAll
                    waspCliTaskList
                    ["addTask", "Adds a task with the given description.", "assertTasks", "failTask"]
                ]
            ]
        ),
      TestCase
        "run-tasks"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile taskPrismaModel,
                  appendToFile ".env.server" "TASK_TEST_VAR=fromEnvServer",
                  createFileInWaspProject [relfile|src/tasks.ts|] tasksFile,
                  replaceMainWaspTsFile mainWaspTs,
                  waspCliCompile,
                  waspCliDbMigrateDev "foo",
                  -- The task gets the arguments after its name, flags included.
                  waspCliTaskRun "addTask" ["--done", "Buy", "milk"],
                  waspCliTaskRun "assertTasks" ["'Buy milk'"],
                  waspCliTaskRunFails "assertTasks" ["'Something else'"],
                  waspCliTaskRunFails "failTask" [],
                  waspCliTaskRunFails "nonExistentTask" []
                ]
            ]
        )
    ]
  where
    waspCliTaskRunFails :: String -> [String] -> ShellCommandBuilder WaspProjectContext ShellCommand
    waspCliTaskRunFails taskName taskArgs = ("! " <>) <$> waspCliTaskRun taskName taskArgs

    waspCliTaskListOutsideProjectFails :: ShellCommand
    waspCliTaskListOutsideProjectFails = "! $WASP_CLI_CMD task list"

    waspCliTaskRunOutsideProjectFails :: ShellCommand
    waspCliTaskRunOutsideProjectFails = "! $WASP_CLI_CMD task run addTask"

    taskPrismaModel :: T.Text
    taskPrismaModel =
      [trimming|
        model Task {
          id          Int     @id @default(autoincrement())
          description String
          isDone      Boolean @default(false)
        }
      |]

    tasksFile :: T.Text
    tasksFile =
      [trimming|
        import fs from 'node:fs'
        import { parseArgs } from 'node:util'
        import type { AddTask, AssertTasks } from 'wasp/server/tasks'

        export const addTask: AddTask = async (args, context) => {
          const { values, positionals } = parseArgs({
            args,
            allowPositionals: true,
            options: { done: { type: 'boolean' } },
          })
          await context.entities.Task.create({
            data: { description: positionals.join(' '), isDone: values.done ?? false },
          })
        }

        export const assertTasks: AssertTasks = async ([expectedDescription], context) => {
          if (!fs.existsSync('main.wasp.ts')) {
            throw new Error(`Expected to run from the Wasp project dir, but ran from $${process.cwd()}`)
          }
          if (process.env.TASK_TEST_VAR !== 'fromEnvServer') {
            throw new Error(`Expected TASK_TEST_VAR from .env.server, but got $${process.env.TASK_TEST_VAR}`)
          }
          const tasks = await context.entities.Task.findMany()
          if (tasks.length !== 1 || tasks[0].description !== expectedDescription || !tasks[0].isDone) {
            throw new Error(`Unexpected tasks: $${JSON.stringify(tasks)}`)
          }
        }

        export async function failTask() {
          throw new Error('This task always fails')
        }
      |]

    mainWaspTs :: T.Text
    mainWaspTs =
      [trimming|
        import { app, page, route, task } from "@wasp/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" };
        import { addTask, assertTasks, failTask } from "./src/tasks" with { type: "ref" };

        export default app({
          name: "waspTaskTest",
          title: "waspTaskTest",
          wasp: { version: "$textWaspVersion" },
          head: ["<link rel='icon' href='/favicon.ico' />"],
          spec: [
            route("RootRoute", "/", page(MainPage)),
            task(addTask, {
              entities: ["Task"],
              description: "Adds a task with the given description.",
            }),
            task(assertTasks, { entities: ["Task"] }),
            task(failTask),
          ]
        })
      |]

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion
