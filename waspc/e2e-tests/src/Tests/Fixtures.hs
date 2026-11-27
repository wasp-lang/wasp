-- | Project files, and the steps that write them, shared by several tests.
--
-- Fixtures edit the starter's files instead of rewriting them, so the tests
-- keep up with the starter. An edit fails loudly when the starter changes.
module Tests.Fixtures
  ( withKitchenSinkProject,
    starterMainPageFile,
    tasksQueryFile,
    taskPrismaModel,
    userCodeTypeError,
    userCodeTypeErrorMessage,
    setRootRoutePath,
    addTasksQueryToMainWaspTs,
    writeTasksQueryApp,
    breakTasksQueryType,
    fixTasksQueryType,
    tasksQueryTypeError,
    Seed (..),
    makeSeed,
    populateTasksSeed,
    assertTasksEmptySeed,
    assertTasksNotEmptySeed,
    allTaskSeeds,
    setUpProjectWithTaskSeeds,
    runSeed,
    addSeeds,
  )
where

import Control.Monad (forM_, void)
import qualified Data.Text as T
import E2E
import NeatInterpolation (trimming)
import Wasp.Project.Db (databaseUrlEnvVarName)

-- | Runs the steps in a copy of the kitchen-sink example app, the one app that
-- uses every Wasp feature. It's set up to compile, but nothing is installed.
withKitchenSinkProject :: TestM a -> TestM a
withKitchenSinkProject steps = do
  copyGitTrackedDir "examples/kitchen-sink" waspAppDirName
  inDirectory waspAppDirName $ do
    copyFile ".env.server.example" dotEnvServerFile
    appendToFile dotEnvServerFile ("\n" <> T.pack databaseUrlEnvVarName <> "=mock-database-url")
    steps

-- | The file of the starter's only page.
starterMainPageFile :: FilePath
starterMainPageFile = "src/MainPage.tsx"

-- | Where 'addTasksQueryToMainWaspTs' expects the @getTasks@ query.
tasksQueryFile :: FilePath
tasksQueryFile = "src/queries.ts"

taskPrismaModel :: T.Text
taskPrismaModel =
  [trimming|
    model Task {
      id          Int     @id @default(autoincrement())
      description String
      isDone      Boolean @default(false)
    }
  |]

-- | A TypeScript statement that doesn't type check, for any of the project's
-- TS files. @tsc@ reports it with 'userCodeTypeErrorMessage'.
userCodeTypeError :: T.Text
userCodeTypeError = "export const shouldBeNumber: number = 'wrong'"

-- | How @tsc@ reports 'userCodeTypeError', without the location.
userCodeTypeErrorMessage :: T.Text
userCodeTypeErrorMessage = "error TS2322: Type 'string' is not assignable to type 'number'."

-- | Replaces the path of the starter's root route with the TypeScript
-- expression.
setRootRoutePath :: T.Text -> TestM ()
setRootRoutePath pathExpression =
  replaceInFile mainWaspTsFile starterRootRoute ("    route(\"RootRoute\", " <> pathExpression <> ", page(MainPage)),")

-- | Declares the @getTasks@ query from @src/queries.ts@ in the starter's
-- @main.wasp.ts@.
addTasksQueryToMainWaspTs :: TestM ()
addTasksQueryToMainWaspTs = do
  -- A separate import, so that we don't depend on what the starter imports.
  addMainWaspTsImports
    [ "import { query } from \"@wasp/spec\";",
      "import { getTasks } from \"./src/queries\" with { type: \"ref\" };"
    ]
  addMainWaspTsSpecDeclaration "query(getTasks)"

-- | An app whose main page renders the result of the @getTasks@ query.
-- The query returns @[{ id: 1 }]@ until changed with 'breakTasksQueryType'.
writeTasksQueryApp :: TestM ()
writeTasksQueryApp = do
  addTasksQueryToMainWaspTs
  writeTasksQuery "[{ id: 1 }]"
  writeToFile
    starterMainPageFile
    [trimming|
      import { getTasks, useQuery } from "wasp/client/operations";

      export function MainPage() {
        const { data: tasks } = useQuery(getTasks);
        return <div>{tasks?.map((task) => <p key={task.id}>{task.id}</p>)}</div>;
      }
    |]

-- | Makes the @getTasks@ query return @[]@, whose type is @never[]@, so the
-- main page of 'writeTasksQueryApp' fails to type check with
-- 'tasksQueryTypeError'.
breakTasksQueryType :: TestM ()
breakTasksQueryType = writeTasksQuery "[]"

-- | Undoes 'breakTasksQueryType'.
fixTasksQueryType :: TestM ()
fixTasksQueryType = writeTasksQuery "[{ id: 1 }]"

tasksQueryTypeError :: T.Text
tasksQueryTypeError = "Property 'id' does not exist on type 'never'."

writeTasksQuery :: T.Text -> TestM ()
writeTasksQuery result =
  writeToFile
    tasksQueryFile
    [trimming|
      import type { GetTasks } from "wasp/server/operations";

      export const getTasks = (async () => {
        return $result;
      }) satisfies GetTasks<void>;
    |]

-- | A seed: a function in @src/db/<name>.ts@ with the seed's name, listed in
-- the app's seeds.
data Seed = Seed {name :: String, script :: T.Text}

-- | A seed whose script has the imports, and whose function runs the body.
makeSeed :: String -> T.Text -> T.Text -> Seed
makeSeed seedName imports body =
  Seed
    { name = seedName,
      script =
        [trimming|
          $imports

          export async function $seedNameText() {
            $body
          }
        |]
    }
  where
    seedNameText = T.pack seedName

-- | Seeds that act on the Task table, or check its state.
--
-- FIXME: Find a way without seeds.
-- Seeds are the only database agnostic way we found to act on the database or
-- check its state through Wasp, without extra dependencies. They can only
-- report success or failure, but that's enough. Running SQL with
-- `prisma db execute` would also work, but it's less readable and not
-- database agnostic.
populateTasksSeed, assertTasksEmptySeed, assertTasksNotEmptySeed :: Seed
populateTasksSeed =
  makeTaskSeed
    "populateTasks"
    [trimming|
      await prisma.task.create({
        data: { description: 'Test task', isDone: false }
      })
    |]
assertTasksEmptySeed =
  makeTaskSeed
    "assertTasksEmpty"
    [trimming|
      const taskCount = await prisma.task.count()
      if (taskCount !== 0) {
        throw new Error(`Expected tasks table to be empty, but found $${taskCount} tasks`)
      }
    |]
assertTasksNotEmptySeed =
  makeTaskSeed
    "assertTasksNotEmpty"
    [trimming|
      const taskCount = await prisma.task.count()
      if (taskCount === 0) {
        throw new Error('Expected tasks table to have data, but it was empty')
      }
    |]

allTaskSeeds :: [Seed]
allTaskSeeds = [populateTasksSeed, assertTasksEmptySeed, assertTasksNotEmptySeed]

makeTaskSeed :: String -> T.Text -> Seed
makeTaskSeed seedName = makeSeed seedName "import { prisma } from 'wasp/server'"

-- | Adds the Task model to the project and migrates the database, then makes
-- the seeds the app's only seeds.
setUpProjectWithTaskSeeds :: [Seed] -> TestM ()
setUpProjectWithTaskSeeds seeds = do
  wasp ["compile"]
  appendToFile prismaSchemaFile taskPrismaModel
  waspDbMigrateDev "add_task_model"
  addSeeds seeds

-- | Runs the seed with `wasp db seed`. Expects 'addSeeds' to have added it.
runSeed :: Seed -> TestM ()
runSeed seed = void $ waspDbSeed [seed.name]

-- | Writes each seed's script to @src/db/<name>.ts@, and makes the seeds the
-- app's only seeds in the starter's @main.wasp.ts@.
addSeeds :: [Seed] -> TestM ()
addSeeds seeds = do
  forM_ seeds $ \seed -> writeToFile ("src/db/" ++ seed.name ++ ".ts") seed.script
  addMainWaspTsImports
    [ "import { " <> seedName <> " } from \"./src/db/" <> seedName <> "\" with { type: \"ref\" };"
    | seedName <- seedNames
    ]
  addMainWaspTsAppField $ "db: { seeds: [" <> T.intercalate ", " seedNames <> "] }"
  where
    seedNames = map (T.pack . (.name)) seeds

addMainWaspTsImports :: [T.Text] -> TestM ()
addMainWaspTsImports imports =
  replaceInFile mainWaspTsFile starterLastImport (T.intercalate "\n" $ starterLastImport : imports)
  where
    starterLastImport = "import { MainPage } from \"./src/MainPage\" with { type: \"ref\" };"

-- | Adds a field to the object passed to @app()@, before @spec@.
addMainWaspTsAppField :: T.Text -> TestM ()
addMainWaspTsAppField field = replaceInFile mainWaspTsFile "  spec: [" ("  " <> field <> ",\n  spec: [")

addMainWaspTsSpecDeclaration :: T.Text -> TestM ()
addMainWaspTsSpecDeclaration declaration =
  replaceInFile mainWaspTsFile starterRootRoute (starterRootRoute <> "\n    " <> declaration <> ",")

starterRootRoute :: T.Text
starterRootRoute = "    route(\"RootRoute\", \"/\", page(MainPage)),"
