-- | Project files shared by several tests.
module Tests.Fixtures
  ( taskPrismaModel,
    writeTasksQueryApp,
    writeTasksQuery,
    TaskSeed (..),
    taskSeedName,
    writeTaskSeedsApp,
  )
where

import Control.Monad (forM_)
import qualified Data.Text as T
import E2E
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

-- | Seeds that act on the Task table, or check its state.
--
-- Seeds are the only database agnostic way we found to act on the database or
-- check its state through Wasp, without extra dependencies. They can only
-- report success or failure, but that's enough.
data TaskSeed = PopulateTasks | AssertTasksEmpty | AssertTasksNotEmpty

taskSeedName :: TaskSeed -> String
taskSeedName = \case
  PopulateTasks -> "populateTasks"
  AssertTasksEmpty -> "assertTasksEmpty"
  AssertTasksNotEmpty -> "assertTasksNotEmpty"

-- | Adds the seeds to the project, and makes them the app's only seeds.
-- Expects the Task model to be in the Prisma schema.
writeTaskSeedsApp :: [TaskSeed] -> TestM ()
writeTaskSeedsApp seeds = do
  forM_ seeds $ \seed ->
    writeToFile ("src/db/" ++ taskSeedName seed ++ ".ts") $ seedScript (T.pack $ taskSeedName seed) (seedBody seed)
  let seedImports =
        T.unlines
          [ "import { " <> name <> " } from \"./src/db/" <> name <> "\" with { type: \"ref\" };"
          | name <- seedNames
          ]
      seedList = T.intercalate ", " seedNames
  writeToFile
    "main.wasp.ts"
    [trimming|
      import { app, page, route } from "@wasp/spec";
      import { MainPage } from "./src/MainPage" with { type: "ref" };
      $seedImports
      export default app({
        name: "waspDbSeedTest",
        title: "waspDbSeedTest",
        wasp: { version: "$textWaspVersion" },
        head: ["<link rel='icon' href='/favicon.ico' />"],
        db: {
          seeds: [$seedList]
        },
        spec: [
          route("RootRoute", "/", page(MainPage)),
        ]
      })
    |]
  where
    seedNames = map (T.pack . taskSeedName) seeds

    seedBody :: TaskSeed -> T.Text
    seedBody = \case
      PopulateTasks ->
        [trimming|
          await prisma.task.create({
            data: { description: 'Test task', isDone: false }
          })
        |]
      AssertTasksEmpty ->
        [trimming|
          const taskCount = await prisma.task.count()
          if (taskCount !== 0) {
            throw new Error(`Expected tasks table to be empty, but found $${taskCount} tasks`)
          }
        |]
      AssertTasksNotEmpty ->
        [trimming|
          const taskCount = await prisma.task.count()
          if (taskCount === 0) {
            throw new Error('Expected tasks table to have data, but it was empty')
          }
        |]

    seedScript :: T.Text -> T.Text -> T.Text
    seedScript name body =
      [trimming|
        import { prisma } from 'wasp/server'

        export async function $name() {
          $body
        }
      |]
