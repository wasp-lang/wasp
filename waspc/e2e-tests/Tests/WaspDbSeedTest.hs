module Tests.WaspDbSeedTest (waspDbSeedTest) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import E2E
import NeatInterpolation (trimming)
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing, renameDirectory)
import System.FilePath (takeDirectory, (</>))
import Tests.Fixtures (TaskSeed (..), addSeedsToMainWaspTs, allTaskSeeds, runTaskSeed, setUpProjectWithTaskSeeds, taskPrismaModel)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInGeneratedAppDir)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

waspDbSeedTest :: E2ETest
waspDbSeedTest =
  testGroup
    "wasp-db-seed"
    [ test "fail-outside-project" $ waspFails ["db", "seed"],
      test "succeed-seed-name-given-multiple-seeds" $
        withNewWaspProject minimalStarterTemplate $ do
          setUpProjectWithTaskSeeds allTaskSeeds
          runTaskSeed AssertTasksEmpty
          runTaskSeed PopulateTasks
          runTaskSeed AssertTasksNotEmpty,
      test "succeed-no-seed-name-single-seed" $
        withNewWaspProject minimalStarterTemplate $ do
          setUpProjectWithTaskSeeds [PopulateTasks]
          wasp ["db", "seed"],
      -- With more than one seed and no name, `wasp db seed` has to ask which
      -- one to run. Without an interactive terminal it must fail instead.
      test "fail-no-seed-name-multiple-seeds-non-interactive" $
        withNewWaspProject minimalStarterTemplate $ do
          setUpProjectWithTaskSeeds allTaskSeeds
          assertFailsWithoutInteractiveTerminal "wasp db seed" $ waspCli ["db", "seed"],
      test "succeed-seed-from-project-dir-with-server-env" $
        withNewWaspProject minimalStarterTemplate $ do
          appendToFile "schema.prisma" taskPrismaModel
          appendToFile ".env.server" "SEED_TEST_VAR=fromEnvServer"
          writeToFile ("src/db/" ++ serverEnvSeedName ++ ".ts") serverEnvSeedScript
          addSeedsToMainWaspTs [serverEnvSeedName]
          wasp ["compile"]
          waspDbMigrateDev "foo"
          replaceRootDotenvWithDecoy
          wasp ["db", "seed", serverEnvSeedName]
    ]
  where
    -- npm usually hoists `dotenv` to the project root's `node_modules`. We move it
    -- into the server's `node_modules` and leave a decoy in its place, so the seed
    -- only passes if the server loads `dotenv` from the server package. The decoy
    -- still provides `parse`, which the Prisma CLI uses to check the db connection.
    replaceRootDotenvWithDecoy :: TestM ()
    replaceRootDotenvWithDecoy = do
      deleteDirectory serverDotenvDir
      rootDotenvAbsDir <- resolvePath rootDotenvDir
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

    rootDotenvDir = "node_modules/dotenv"
    serverDotenvDir = SP.fromRelDir (generatedAppDirInWaspProjectDir SP.</> serverRootDirInGeneratedAppDir) </> "node_modules/dotenv"
    serverDotenvDirText = T.pack serverDotenvDir

    serverEnvSeedName :: String
    serverEnvSeedName = "assertServerEnv"

    serverEnvSeedScript :: T.Text
    serverEnvSeedScript =
      [trimming|
        import fs from 'node:fs'

        export async function $serverEnvSeedNameText() {
          if (!fs.existsSync('main.wasp.ts')) {
            throw new Error(`Expected to run from the Wasp project dir, but ran from $${process.cwd()}`)
          }
          if (process.env.SEED_TEST_VAR !== 'fromEnvServer') {
            throw new Error(`Expected SEED_TEST_VAR from .env.server, but got $${process.env.SEED_TEST_VAR}`)
          }
        }
      |]
      where
        serverEnvSeedNameText = T.pack serverEnvSeedName
