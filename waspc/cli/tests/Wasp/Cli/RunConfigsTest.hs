module Wasp.Cli.RunConfigsTest where

import StrongPath (absdirP)
import Test.Hspec
import Wasp.AppComponentUrl (AppComponentUrl (..))
import Wasp.AppSpec.App.Deployment (DeploymentMode (..))
import Wasp.Cli.RunConfigs (makeRunConfigs, showAppUrl, showRunConfigUrls)

spec_showRunConfigUrls :: Spec
spec_showRunConfigUrls = do
  it "in single deployment mode shows the app URL and the server URL for debugging" $ do
    showRunConfigUrls Single (makeRunConfigs Single urls)
      `shouldBe` unlines
        [ " ℹ App:    http://localhost:3000/app/",
          "   For debugging purposes, you can reach the server directly here:",
          " ℹ Server: http://localhost:3001/"
        ]

  it "in split mode shows the client and the server" $ do
    showRunConfigUrls Split (makeRunConfigs Split urls)
      `shouldBe` unlines
        [ " ℹ Client: http://localhost:3000/app/",
          " ℹ Server: http://localhost:3001/"
        ]

  it "shows just the app URL" $ do
    showAppUrl (fst $ makeRunConfigs Single urls)
      `shouldBe` " ℹ App: http://localhost:3000/app/"
  where
    urls = (Local {port = 3000, path = Just [absdirP|/app/|]}, Local {port = 3001, path = Nothing})
