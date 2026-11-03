module Tests.WaspDbSeedTest (waspDbSeedTest) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import E2e
import NeatInterpolation (trimming)
import System.Directory (createDirectoryIfMissing, removePathForcibly, renameDirectory)
import System.FilePath (takeDirectory)
import Tests.Fixtures (assertTasksEmptySeed, assertTasksNotEmptySeed, populateTasksSeed, taskPrismaModel, writeTaskSeedsApp)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspDbSeedTest :: E2eTest
waspDbSeedTest =
  testGroup
    "wasp-db-seed"
    [ test "fail-outside-project" $ waspFails ["db", "seed"],
      test "succeed-seed-database" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          appendToFile "schema.prisma" taskPrismaModel
          waspDbMigrateDev "foo"
          writeTaskSeedsApp
          wasp ["db", "seed", assertTasksEmptySeed]
          wasp ["db", "seed", populateTasksSeed]
          wasp ["db", "seed", assertTasksNotEmptySeed],
      test "succeed-seed-from-project-dir-with-server-env" $
        withNewWaspProject minimalStarterTemplate $ do
          appendToFile "schema.prisma" taskPrismaModel
          appendToFile ".env.server" "SEED_TEST_VAR=fromEnvServer"
          writeToFile ("src/db/" ++ T.unpack assertServerEnvSeed ++ ".ts") assertServerEnvSeedScript
          writeToFile "main.wasp.ts" mainWaspTsWithServerEnvSeed
          wasp ["compile"]
          waspDbMigrateDev "foo"
          replaceRootDotenvWithDecoy
          wasp ["db", "seed", T.unpack assertServerEnvSeed]
    ]
  where
    -- npm usually hoists `dotenv` to the project root's `node_modules`. We move it
    -- into the server's `node_modules` and leave a decoy in its place, so the seed
    -- only passes if the server loads `dotenv` from the server package. The decoy
    -- still provides `parse`, which the Prisma CLI uses to check the db connection.
    replaceRootDotenvWithDecoy :: TestM ()
    replaceRootDotenvWithDecoy = do
      rootDotenvDir <- resolvePath "node_modules/dotenv"
      serverDotenvDir <- resolvePath ".wasp/out/server/node_modules/dotenv"
      liftIO $ do
        createDirectoryIfMissing True (takeDirectory serverDotenvDir)
        removePathForcibly serverDotenvDir
        renameDirectory rootDotenvDir serverDotenvDir
      writeToFile "node_modules/dotenv/package.json" "{\"name\": \"dotenv\"}"
      writeToFile
        "node_modules/dotenv/index.js"
        [trimming|
          exports.parse = require("../../.wasp/out/server/node_modules/dotenv").parse;
          exports.config = () => { throw new Error("Loaded the decoy dotenv"); };
        |]
      writeToFile "node_modules/dotenv/config.js" "throw new Error(\"Loaded the decoy dotenv\");"

    assertServerEnvSeed :: T.Text
    assertServerEnvSeed = "assertServerEnv"

    assertServerEnvSeedScript :: T.Text
    assertServerEnvSeedScript =
      [trimming|
        import fs from 'node:fs'

        export async function $assertServerEnvSeed() {
          if (!fs.existsSync('main.wasp.ts')) {
            throw new Error(`Expected to run from the Wasp project dir, but ran from $${process.cwd()}`)
          }
          if (process.env.SEED_TEST_VAR !== 'fromEnvServer') {
            throw new Error(`Expected SEED_TEST_VAR from .env.server, but got $${process.env.SEED_TEST_VAR}`)
          }
        }
      |]

    mainWaspTsWithServerEnvSeed :: T.Text
    mainWaspTsWithServerEnvSeed =
      [trimming|
        import { app, page, route } from "@wasp/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" };
        import { $assertServerEnvSeed } from "./src/db/$assertServerEnvSeed" with { type: "ref" };

        export default app({
          name: "waspDbSeedTest",
          title: "waspDbSeedTest",
          wasp: { version: "$textWaspVersion" },
          head: ["<link rel='icon' href='/favicon.ico' />"],
          db: {
            seeds: [$assertServerEnvSeed]
          },
          spec: [
            route("RootRoute", "/", page(MainPage)),
          ]
        })
      |]
