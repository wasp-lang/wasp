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
  writeMainWaspTs
    [trimming|
      import { app, page, query, route } from "@wasp.sh/spec";
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

-- | Adds the Task model and the seeds to the project.
--
-- Seeds are the only database agnostic way we found to act on the database or
-- check its state through Wasp, without extra dependencies. They can only
-- report success or failure, but that's enough.
writeTaskSeedsApp :: TestM ()
writeTaskSeedsApp = do
  appendToPrismaSchema taskPrismaModel
  writeSeedFile (populateTasksSeed ++ ".ts") $
    seedScript
      populateTasksSeed
      "await prisma.task.create({ data: { description: 'Test task', isDone: false } })"
  writeSeedFile (assertTasksEmptySeed ++ ".ts") $
    seedScript
      assertTasksEmptySeed
      "if ((await prisma.task.count()) !== 0) throw new Error('Expected tasks table to be empty')"
  writeSeedFile (assertTasksNotEmptySeed ++ ".ts") $
    seedScript
      assertTasksNotEmptySeed
      "if ((await prisma.task.count()) === 0) throw new Error('Expected tasks table to have data, but it was empty')"
  writeMainWaspTs
    [trimming|
      import { app, page, route } from "@wasp.sh/spec";
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
          seeds: [$populate, $assertEmpty, $assertNotEmpty]
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

    seedScript :: String -> T.Text -> T.Text
    seedScript seedName body =
      let name = T.pack seedName
       in [trimming|
            import { prisma } from 'wasp/server'

            export async function $name() {
              $body
            }
          |]

populateTasksSeed, assertTasksEmptySeed, assertTasksNotEmptySeed :: String
populateTasksSeed = "populateTasks"
assertTasksEmptySeed = "assertTasksEmpty"
assertTasksNotEmptySeed = "assertTasksNotEmpty"
