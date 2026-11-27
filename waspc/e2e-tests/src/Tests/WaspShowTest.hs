{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Tests.WaspShowTest (test_waspShow) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON)
import qualified Data.Text as T
import E2E
import GHC.Generics (Generic)
import Tests.Fixtures (setRootRoutePath)
import UnliftIO.Async (mapConcurrently)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.WaspInfo (WaspInfo)

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
          spec :: ShowSpecJson <- wasp ["show", "spec", "--json"] >>= decodeStdoutJson
          spec.waspVersion `shouldBe` textWaspVersion
          wasp ["show", "build"] >>= assertStdoutContains "Project dir size"
          build :: ShowBuildJson <- wasp ["show", "build", "--json"] >>= decodeStdoutJson
          -- The project was never compiled.
          build.lastCompile `shouldBe` Nothing,
      -- `wasp show spec` doesn't lock the project, so its analysis can overlap
      -- with others (e.g. a `wasp start` recompile). Each analysis here
      -- declares a different route (taken from an env var), and must only see
      -- its own.
      test "concurrent-spec-analyses-read-their-own-results" $
        withNewWaspProject minimalStarterTemplate $ do
          setRootRoutePath $ "`/${process.env." <> T.pack routeEnvVarName <> "}`"
          let routes = ["e2e-route-" ++ show index | index <- [1 .. 8 :: Int]]
          results <-
            mapConcurrently
              (\route -> run $ withEnv [(routeEnvVarName, route)] $ waspCli ["show", "spec", "--json"])
              routes
          forM_ (zip routes results) $ \(route, result) -> do
            spec :: ShowSpecJson <- decodeStdoutJson result
            [declValue.path | DeclJson {declType = "Route", declValue} <- spec.decls] `shouldBe` [Just ("/" ++ route)]
    ]
  where
    routeEnvVarName = "E2E_ROUTE"

-- | What the tests check of what `wasp show spec --json` prints.
data ShowSpecJson = ShowSpecJson
  { waspVersion :: T.Text,
    decls :: [DeclJson]
  }
  deriving (Generic, FromJSON)

data DeclJson = DeclJson
  { declType :: String,
    declValue :: DeclValueJson
  }
  deriving (Generic, FromJSON)

-- | Each type of declaration has its own fields. Routes have a path.
newtype DeclValueJson = DeclValueJson {path :: Maybe String}
  deriving (Generic, FromJSON)

-- | What the tests check of what `wasp show build --json` prints.
newtype ShowBuildJson = ShowBuildJson {lastCompile :: Maybe WaspInfo}
  deriving (Generic, FromJSON)
