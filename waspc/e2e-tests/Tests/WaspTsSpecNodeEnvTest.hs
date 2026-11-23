module Tests.WaspTsSpecNodeEnvTest (waspTsSpecNodeEnvTest) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspTsSpecNodeEnvTest :: E2ETest
waspTsSpecNodeEnvTest =
  testGroup
    "wasp-ts-spec-node-env"
    [ test "node-env-is-development-on-compile" $
        withNewWaspProject minimalStarterTemplate $ do
          logNodeEnvInMainWaspTs
          wasp ["compile"] >>= assertOutputContains "E2E-NODE-ENV=development",
      test "node-env-is-production-on-build" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          logNodeEnvInMainWaspTs
          wasp ["build"] >>= assertOutputContains "E2E-NODE-ENV=production"
    ]
  where
    logNodeEnvInMainWaspTs :: TestM ()
    logNodeEnvInMainWaspTs = appendToFile "main.wasp.ts" "console.log(`E2E-NODE-ENV=${process.env.NODE_ENV}`);"
