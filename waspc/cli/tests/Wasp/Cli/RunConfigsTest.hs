module Wasp.Cli.RunConfigsTest where

import StrongPath (absdirP)
import Test.Hspec
import Wasp.AppComponentUrl (AppComponentUrl (..))
import Wasp.AppSpec.App.Deployment (DeploymentMode (..))
import Wasp.Cli.RunConfigs (makeRunConfigs, showRunConfigUrls)
import Wasp.Project.BuildType (BuildType (..))

spec_showRunConfigUrls :: Spec
spec_showRunConfigUrls = do
  it "in single deployment mode shows the app URL and the server URL for debugging" $ do
    showRunConfigUrls Development Single (makeRunConfigs Single urls)
      `shouldBe` unlines
        [ " ℹ App:    http://localhost:3000/app/",
          "   For debugging purposes, you can reach the server directly here:",
          " ℹ Server: http://localhost:3001/"
        ]

  it "in split mode shows the client and the server" $ do
    showRunConfigUrls Development Split (makeRunConfigs Split urls)
      `shouldBe` unlines
        [ " ℹ Client: http://localhost:3000/app/",
          " ℹ Server: http://localhost:3001/"
        ]

  it "in single deployment mode in production shows only the app URL" $ do
    showRunConfigUrls Production Single (makeRunConfigs Single urls)
      `shouldBe` unlines
        [ " ℹ App:    http://localhost:3000/app/"
        ]
  where
    urls = (Local {port = 3000, path = Just [absdirP|/app/|]}, Local {port = 3001, path = Nothing})
