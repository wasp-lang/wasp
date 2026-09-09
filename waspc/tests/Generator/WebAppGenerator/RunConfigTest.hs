module Generator.WebAppGenerator.RunConfigTest where

import StrongPath (absdirP)
import Test.Hspec
import Wasp.AppComponentUrl (AppComponentUrl (..))
import Wasp.AppSpec.App.Deployment (DeploymentMode (..))
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..), makeWebAppRunConfig)

spec_makeWebAppRunConfig :: Spec
spec_makeWebAppRunConfig = do
  it "in single deployment mode points the dev proxy at the server origin" $ do
    envVars (makeWebAppRunConfig Single clientUrl serverUrl)
      `shouldBe` [("WASP_DEV_PROXY_TARGET", "http://localhost:3001"), ("PORT", "3000")]

  it "in split mode points the client at the server URL" $ do
    envVars (makeWebAppRunConfig Split clientUrl serverUrl)
      `shouldBe` [("REACT_APP_API_URL", "http://localhost:3001"), ("PORT", "3000")]

  it "keeps the client URL, base dir included" $ do
    url (makeWebAppRunConfig Single clientUrl serverUrl) `shouldBe` clientUrl
  where
    clientUrl = Local {port = 3000, path = Just [absdirP|/app/|]}
    serverUrl = Local {port = 3001, path = Nothing}
