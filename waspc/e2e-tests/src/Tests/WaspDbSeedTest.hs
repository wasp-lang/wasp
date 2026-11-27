module Tests.WaspDbSeedTest (test_waspDbSeed) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import E2E
import NeatInterpolation (trimming)
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing, renameDirectory)
import System.FilePath (takeDirectory, (</>))
import Tests.Fixtures
  ( Seed (..),
    addSeeds,
    allTaskSeeds,
    assertTasksEmptySeed,
    assertTasksNotEmptySeed,
    makeSeed,
    populateTasksSeed,
    runSeed,
    setUpProjectWithTaskSeeds,
    taskPrismaModel,
  )
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInGeneratedAppDir)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

test_waspDbSeed :: TestTree
test_waspDbSeed =
  testGroup
    "wasp-db-seed"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["db", "seed"],
      test "succeed-seed-name-given-multiple-seeds" $
        withNewWaspProject minimalStarterTemplate $ do
          setUpProjectWithTaskSeeds allTaskSeeds
          runSeed assertTasksEmptySeed
          runSeed populateTasksSeed
          runSeed assertTasksNotEmptySeed,
      test "succeed-no-seed-name-single-seed" $
        withNewWaspProject minimalStarterTemplate $ do
          setUpProjectWithTaskSeeds [populateTasksSeed]
          waspDbSeed [] >>= assertOutputContains (T.pack $ "Running database seed " ++ populateTasksSeed.name ++ "..."),
      -- With more than one seed and no name, `wasp db seed` has to ask which
      -- one to run. Without an interactive terminal it must fail instead.
      test "fail-no-seed-name-multiple-seeds-non-interactive" $
        withNewWaspProject minimalStarterTemplate $ do
          setUpProjectWithTaskSeeds allTaskSeeds
          assertFailsWithoutInteractiveTerminal "wasp db seed" $ waspCli ["db", "seed"],
      test "succeed-seed-from-project-dir-with-server-env" $
        withNewWaspProject minimalStarterTemplate $ do
          -- The seed doesn't use the Task model, but without a model, the
          -- seed runner Wasp generates doesn't type check (`prisma` can be
          -- null).
          appendToFile prismaSchemaFile taskPrismaModel
          writeToFile dotEnvServerFile "SEED_TEST_VAR=fromEnvServer"
          addSeeds [serverEnvSeed]
          -- `wasp db seed` compiles too, but we need the server's dependencies
          -- (with `dotenv`) installed before we replace it. The seed doesn't
          -- need a migrated database, but we seed one like a real project
          -- would.
          wasp ["compile"]
          waspDbMigrateDev "add_task_model"
          replaceRootDotenvWithDecoy
          waspDbSeed [serverEnvSeed.name]
    ]
  where
    -- npm usually hoists `dotenv` to the project root's `node_modules`. We move it
    -- into the server's `node_modules` and leave a decoy in its place, so the seed
    -- only passes if the server loads `dotenv` from the server package. The decoy
    -- still provides `parse`, which the Prisma CLI uses to check the db connection.
    replaceRootDotenvWithDecoy :: TestM ()
    replaceRootDotenvWithDecoy = do
      -- npm must have hoisted it. We check before deleting the server's copy,
      -- so that the test fails on why it can't continue.
      assertDirectoryExists rootDotenvDir
      rootDotenvAbsDir <- resolvePath rootDotenvDir
      deleteDirectory serverDotenvDir
      serverDotenvAbsDir <- resolvePath serverDotenvDir
      liftIO $ do
        createDirectoryIfMissing True (takeDirectory serverDotenvAbsDir)
        renameDirectory rootDotenvAbsDir serverDotenvAbsDir
      writeToFile (rootDotenvDir </> "package.json") "{\"name\": \"dotenv\"}"
      writeToFile
        (rootDotenvDir </> "index.js")
        [trimming|
          exports.parse = require("../../$serverDotenvDirText").parse;
          exports.config = () => { throw new Error("Loaded the decoy dotenv"); };
        |]
      writeToFile (rootDotenvDir </> "config.js") "throw new Error(\"Loaded the decoy dotenv\");"

    -- Two levels deep: the decoy's `require("../../...")` relies on it.
    rootDotenvDir = "node_modules/dotenv"
    serverDotenvDir = SP.fromRelDir (generatedAppDirInWaspProjectDir SP.</> serverRootDirInGeneratedAppDir) </> "node_modules/dotenv"
    serverDotenvDirText = T.pack serverDotenvDir

    serverEnvSeed :: Seed
    serverEnvSeed =
      makeSeed
        "assertServerEnv"
        "import fs from 'node:fs'"
        [trimming|
          if (!fs.existsSync('main.wasp.ts')) {
            throw new Error(`Expected to run from the Wasp project dir, but ran from $${process.cwd()}`)
          }
          if (process.env.SEED_TEST_VAR !== 'fromEnvServer') {
            throw new Error(`Expected SEED_TEST_VAR from .env.server, but got $${process.env.SEED_TEST_VAR}`)
          }
        |]
