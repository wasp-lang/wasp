module Tests.WaspShowTest (waspShowTest) where

import Control.Monad (forM_)
import qualified Data.Text as T
import E2E
import UnliftIO.Async (mapConcurrently)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspShowTest :: E2ETest
waspShowTest =
  testGroup
    "wasp-show"
    [ test "fail-without-subcommand" $ waspFails ["show"],
      test "fail-outside-project" $ do
        waspFails ["show", "spec"]
        waspFails ["show", "build"],
      test "succeed-inside-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["show", "spec"]
          -- With `--json`, stdout alone must be valid JSON.
          wasp ["show", "spec", "--json"] >>= assertValidJson . (.stdout)
          wasp ["show", "build"]
          wasp ["show", "build", "--json"] >>= assertValidJson . (.stdout),
      -- `wasp show spec` doesn't lock the project, so its analysis can overlap
      -- with others (e.g. a `wasp start` recompile). Each analysis here
      -- declares a different route (taken from an env var), and must only see
      -- its own.
      test "concurrent-spec-analyses-read-their-own-results" $
        withNewWaspProject minimalStarterTemplate $ do
          replaceInFile "main.wasp.ts" "route(\"RootRoute\", \"/\"" "route(\"RootRoute\", `/${process.env.E2E_ROUTE}`"
          let routes = ["e2e-route-" ++ show index | index <- [1 .. 8 :: Int]]
          results <-
            mapConcurrently
              (\route -> run $ withEnv [("E2E_ROUTE", route)] $ waspCli ["show", "spec", "--json"])
              routes
          forM_ (zip routes results) $ \(route, result) ->
            assertOutputContains (T.pack $ "/" ++ route ++ "\"") result
    ]
