module Tests.WaspTsSpecNodeEnvTest (test_waspTsSpecNodeEnv) where

import qualified Data.Text as T
import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

test_waspTsSpecNodeEnv :: TestTree
test_waspTsSpecNodeEnv =
  testGroup
    "wasp-ts-spec-node-env"
    [ test "node-env-is-development-on-compile" $
        withNewWaspProject minimalStarterTemplate $ do
          logNodeEnvInMainWaspTs
          wasp ["compile"] >>= assertOutputContains (nodeEnvLogPrefix <> "development"),
      test "node-env-is-production-on-build" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          logNodeEnvInMainWaspTs
          wasp ["build"] >>= assertOutputContains (nodeEnvLogPrefix <> "production")
    ]
  where
    logNodeEnvInMainWaspTs :: TestM ()
    logNodeEnvInMainWaspTs = appendToFile mainWaspTsFile ("console.log(`" <> nodeEnvLogPrefix <> "${process.env.NODE_ENV}`);")

    nodeEnvLogPrefix :: T.Text
    nodeEnvLogPrefix = "E2E-NODE-ENV="
