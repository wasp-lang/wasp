module Tests.WaspTsSpecNodeEnvTest (waspTsSpecNodeEnvTests) where

import qualified Data.Text as T
import E2e
import NeatInterpolation (trimming)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspTsSpecNodeEnvTests :: [E2eTest]
waspTsSpecNodeEnvTests =
  testGroup
    "wasp-ts-spec-node-env"
    [ test "node-env-is-development-on-compile" $
        withNewWaspProject minimalStarterTemplate $ do
          writeMainWaspTs mainWaspTsLoggingNodeEnv
          wasp ["compile"] >>= assertOutputContains "E2E-NODE-ENV=development",
      test "node-env-is-production-on-build" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          writeMainWaspTs mainWaspTsLoggingNodeEnv
          wasp ["build"] >>= assertOutputContains "E2E-NODE-ENV=production"
    ]
  where
    mainWaspTsLoggingNodeEnv :: T.Text
    mainWaspTsLoggingNodeEnv =
      [trimming|
        import { app, page, route } from "@wasp.sh/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" }

        console.log(`E2E-NODE-ENV=$${process.env.NODE_ENV}`);

        export default app({
          name: "tsSpecNodeEnvTest",
          title: "tsSpecNodeEnvTest",
          wasp: { version: "$textWaspVersion" },
          head: ["<link rel='icon' href='/favicon.ico' />"],
          spec: [
            route("RootRoute", "/", page(MainPage)),
          ]
        })
      |]
