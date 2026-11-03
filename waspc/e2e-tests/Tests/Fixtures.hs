-- | Project files shared by several tests.
module Tests.Fixtures
  ( taskPrismaModel,
    writeTasksQueryApp,
    writeTasksQuery,
    writeTaskSeedsApp,
    populateTasksSeed,
    assertTasksEmptySeed,
    assertTasksNotEmptySeed,
  )
where

import qualified Data.Text as T
import E2e
import NeatInterpolation (trimming)

taskPrismaModel :: T.Text
taskPrismaModel =
  [trimming|
    model Task {
      id          Int     @id @default(autoincrement())
      description String
      isDone      Boolean @default(false)
    }
  |]

-- | An app whose main page renders the result of the @getTasks@ query.
-- The query returns @[{ id: 1 }]@ until changed with 'writeTasksQuery'.
writeTasksQueryApp :: TestM ()
writeTasksQueryApp = do
  writeToFile
    "main.wasp.ts"
    [trimming|
      import { app, page, query, route } from "@wasp/spec";
      import { MainPage } from "./src/MainPage" with { type: "ref" };
      import { getTasks } from "./src/queries" with { type: "ref" };

      export default app({
        name: "operationTypes",
        title: "Operation types",
        wasp: { version: "$textWaspVersion" },
        spec: [
          route("RootRoute", "/", page(MainPage)),
          query(getTasks),
        ],
      });
    |]
  writeTasksQuery "[{ id: 1 }]"
  writeToFile
    "src/MainPage.tsx"
    [trimming|
      import { getTasks, useQuery } from "wasp/client/operations";

      export function MainPage() {
        const { data: tasks } = useQuery(getTasks);
        return <div>{tasks?.map((task) => <p key={task.id}>{task.id}</p>)}</div>;
      }
    |]

writeTasksQuery :: T.Text -> TestM ()
writeTasksQuery result =
  writeToFile
    "src/queries.ts"
    [trimming|
      import type { GetTasks } from "wasp/server/operations";

      export const getTasks = (async () => {
        return $result;
      }) satisfies GetTasks<void>;
    |]

-- | Adds seeds that populate the Task table, and check whether it's empty.
--
-- Seeds are the only database agnostic way we found to act on the database or
-- check its state through Wasp, without extra dependencies. They can only
-- report success or failure, but that's enough.
--
-- Expects the Task model to be in the Prisma schema.
writeTaskSeedsApp :: TestM ()
writeTaskSeedsApp = do
  writeToFile ("src/db/" ++ populateTasksSeed ++ ".ts") $
    seedScript
      populate
      [trimming|
        await prisma.task.create({
          data: { description: 'Test task', isDone: false }
        })
      |]
  writeToFile ("src/db/" ++ assertTasksEmptySeed ++ ".ts") $
    seedScript
      assertEmpty
      [trimming|
        const taskCount = await prisma.task.count()
        if (taskCount !== 0) {
          throw new Error(`Expected tasks table to be empty, but found $${taskCount} tasks`)
        }
      |]
  writeToFile ("src/db/" ++ assertTasksNotEmptySeed ++ ".ts") $
    seedScript
      assertNotEmpty
      [trimming|
        const taskCount = await prisma.task.count()
        if (taskCount === 0) {
          throw new Error('Expected tasks table to have data, but it was empty')
        }
      |]
  writeToFile
    "main.wasp.ts"
    [trimming|
      import { app, page, route } from "@wasp/spec";
      import { MainPage } from "./src/MainPage" with { type: "ref" };
      import { $populate } from "./src/db/$populate" with { type: "ref" };
      import { $assertEmpty } from "./src/db/$assertEmpty" with { type: "ref" };
      import { $assertNotEmpty } from "./src/db/$assertNotEmpty" with { type: "ref" };

      export default app({
        name: "waspDbSeedTest",
        title: "waspDbSeedTest",
        wasp: { version: "$textWaspVersion" },
        head: ["<link rel='icon' href='/favicon.ico' />"],
        db: {
          seeds: [
            $populate,
            $assertEmpty,
            $assertNotEmpty
          ]
        },
        spec: [
          route("RootRoute", "/", page(MainPage)),
        ]
      })
    |]
  where
    populate = T.pack populateTasksSeed
    assertEmpty = T.pack assertTasksEmptySeed
    assertNotEmpty = T.pack assertTasksNotEmptySeed

    seedScript :: T.Text -> T.Text -> T.Text
    seedScript name body =
      [trimming|
        import { prisma } from 'wasp/server'

        export async function $name() {
          $body
        }
      |]

populateTasksSeed, assertTasksEmptySeed, assertTasksNotEmptySeed :: String
populateTasksSeed = "populateTasks"
assertTasksEmptySeed = "assertTasksEmpty"
assertTasksNotEmptySeed = "assertTasksNotEmpty"
