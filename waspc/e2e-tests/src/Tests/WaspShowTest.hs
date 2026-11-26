module Tests.WaspShowTest (test_waspShow) where

import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import E2E
import Tests.Fixtures (setRootRoutePath)
import UnliftIO.Async (mapConcurrently)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

test_waspShow :: TestTree
test_waspShow =
  testGroup
    "wasp-show"
    [ test "fail-without-subcommand" $ waspFails ["show"],
      test "fail-outside-project" $ do
        waspFailsOutsideProject ["show", "spec"]
        waspFailsOutsideProject ["show", "build"],
      test "succeed-inside-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["show", "spec"] >>= assertStdoutContains "RootRoute"
          -- With `--json`, stdout alone must be the JSON.
          wasp ["show", "spec", "--json"] >>= assertStdoutJsonField "waspVersion" (Aeson.String textWaspVersion)
          wasp ["show", "build"] >>= assertStdoutContains "Project dir size"
          -- The project was never compiled.
          wasp ["show", "build", "--json"] >>= assertStdoutJsonField "lastCompile" Aeson.Null,
      -- `wasp show spec` doesn't lock the project, so its analysis can overlap
      -- with others (e.g. a `wasp start` recompile). Each analysis here
      -- declares a different route (taken from an env var), and must only see
      -- its own.
      test "concurrent-spec-analyses-read-their-own-results" $
        withNewWaspProject minimalStarterTemplate $ do
          setRootRoutePath "`/${process.env.E2E_ROUTE}`"
          let routes = ["e2e-route-" ++ show index | index <- [1 .. 8 :: Int]]
          results <-
            mapConcurrently
              (\route -> run $ withEnv [("E2E_ROUTE", route)] $ waspCli ["show", "spec", "--json"])
              routes
          forM_ (zip routes results) $ \(route, result) ->
            assertOutputContains (T.pack $ "/" ++ route ++ "\"") result
    ]
