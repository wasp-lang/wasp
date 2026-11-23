-- | Project files and commands shared by several tests.
--
-- Fixtures edit the starter's files instead of rewriting them, so the tests
-- keep up with the starter. An edit fails loudly when the starter changes.
module Tests.Fixtures
  ( taskPrismaModel,
    addTasksQueryToMainWaspTs,
    writeTasksQueryApp,
    writeTasksQuery,
    TaskSeed (..),
    allTaskSeeds,
    setUpProjectWithTaskSeeds,
    runTaskSeed,
    addSeedsToMainWaspTs,
    viteBuild,
    withApiUrl,
  )
where

import Control.Monad (forM_)
import qualified Data.Text as T
import E2E
import NeatInterpolation (trimming)
import Wasp.Generator.WebAppGenerator.Common (serverUrlEnvVarName)

taskPrismaModel :: T.Text
taskPrismaModel =
  [trimming|
    model Task {
      id          Int     @id @default(autoincrement())
      description String
      isDone      Boolean @default(false)
    }
  |]

-- | Declares the @getTasks@ query from @src/queries.ts@ in the starter's
-- @main.wasp.ts@.
addTasksQueryToMainWaspTs :: TestM ()
addTasksQueryToMainWaspTs = do
  replaceInFile "main.wasp.ts" "import { app, page, route } from \"@wasp/spec\";" "import { app, page, query, route } from \"@wasp/spec\";"
  addMainWaspTsImports ["import { getTasks } from \"./src/queries\" with { type: \"ref\" };"]
  addMainWaspTsSpecDeclaration "query(getTasks)"

-- | An app whose main page renders the result of the @getTasks@ query.
-- The query returns @[{ id: 1 }]@ until changed with 'writeTasksQuery'.
writeTasksQueryApp :: TestM ()
writeTasksQueryApp = do
  addTasksQueryToMainWaspTs
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
-- FIXME: Find a way without seeds.
-- Seeds are the only database agnostic way we found to act on the database or
-- check its state through Wasp, without extra dependencies. They can only
-- report success or failure, but that's enough. Running SQL with
-- `prisma db execute` would also work, but it's less readable and not
-- database agnostic.
data TaskSeed = PopulateTasks | AssertTasksEmpty | AssertTasksNotEmpty
  deriving (Bounded, Enum)

allTaskSeeds :: [TaskSeed]
allTaskSeeds = [minBound .. maxBound]

-- | Adds the Task model to the project and migrates the database, then makes
-- the seeds the app's only seeds.
setUpProjectWithTaskSeeds :: [TaskSeed] -> TestM ()
setUpProjectWithTaskSeeds seeds = do
  wasp ["compile"]
  appendToFile "schema.prisma" taskPrismaModel
  waspDbMigrateDev "foo"
  forM_ seeds $ \seed ->
    writeToFile ("src/db/" ++ taskSeedName seed ++ ".ts") $ seedScript (T.pack $ taskSeedName seed) (seedBody seed)
  addSeedsToMainWaspTs $ map taskSeedName seeds
  where
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

-- | Runs the seed with `wasp db seed`. Expects 'setUpProjectWithTaskSeeds' to
-- have added it.
runTaskSeed :: TaskSeed -> TestM ProcessResult
runTaskSeed seed = wasp ["db", "seed", taskSeedName seed]

taskSeedName :: TaskSeed -> String
taskSeedName = \case
  PopulateTasks -> "populateTasks"
  AssertTasksEmpty -> "assertTasksEmpty"
  AssertTasksNotEmpty -> "assertTasksNotEmpty"

-- | Makes the named functions the app's seeds in the starter's @main.wasp.ts@.
-- Each one is the export of the same name in @src/db/<name>.ts@.
addSeedsToMainWaspTs :: [String] -> TestM ()
addSeedsToMainWaspTs seedNames = do
  addMainWaspTsImports
    [ "import { " <> name <> " } from \"./src/db/" <> name <> "\" with { type: \"ref\" };"
    | name <- seedNameTexts
    ]
  addMainWaspTsAppField $ "db: { seeds: [" <> T.intercalate ", " seedNameTexts <> "] }"
  where
    seedNameTexts = map T.pack seedNames

-- | `vite build` in the Wasp project, with the extra arguments.
--
-- Without the test runner's @REACT_APP_API_URL@, so that each test decides
-- whether the client gets it, which it needs to build. Use 'withApiUrl' to
-- set it.
viteBuild :: [String] -> Command
viteBuild arguments = withoutEnv [serverUrlEnvVarName] $ nodeModulesBin "vite" ("build" : arguments)

-- | Sets the server URL the client is built with.
withApiUrl :: Command -> Command
withApiUrl = withEnv [(serverUrlEnvVarName, "http://localhost:3001")]

addMainWaspTsImports :: [T.Text] -> TestM ()
addMainWaspTsImports imports =
  replaceInFile "main.wasp.ts" starterLastImport (T.intercalate "\n" $ starterLastImport : imports)
  where
    starterLastImport = "import { MainPage } from \"./src/MainPage\" with { type: \"ref\" };"

-- | Adds a field to the object passed to @app()@, before @spec@.
addMainWaspTsAppField :: T.Text -> TestM ()
addMainWaspTsAppField field = replaceInFile "main.wasp.ts" "  spec: [" ("  " <> field <> ",\n  spec: [")

addMainWaspTsSpecDeclaration :: T.Text -> TestM ()
addMainWaspTsSpecDeclaration declaration =
  replaceInFile "main.wasp.ts" starterRoute (starterRoute <> "\n    " <> declaration <> ",")
  where
    starterRoute = "    route(\"RootRoute\", \"/\", page(MainPage)),"
