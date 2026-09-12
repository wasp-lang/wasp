module Generator.ServerGenerator.RunConfigTest where

import StrongPath (absdirP)
import Test.Hspec
import Wasp.AppComponentUrl (AppComponentUrl (..))
import Wasp.AppSpec.App.Deployment (DeploymentMode (..))
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..), makeServerRunConfig)

spec_makeServerRunConfig :: Spec
spec_makeServerRunConfig = do
  it "in single deployment mode is reached on the client origin" $ do
    envVars (makeServerRunConfig Single serverUrl clientUrl)
      `shouldBe` [ ("WASP_WEB_CLIENT_URL", "http://localhost:3000/app/"),
                   ("WASP_SERVER_URL", "http://localhost:3000"),
                   ("PORT", "3001")
                 ]

  it "in split mode has its own URL and the client URL with the base dir" $ do
    envVars (makeServerRunConfig Split serverUrl clientUrl)
      `shouldBe` [ ("WASP_WEB_CLIENT_URL", "http://localhost:3000/app/"),
                   ("WASP_SERVER_URL", "http://localhost:3001"),
                   ("PORT", "3001")
                 ]

  it "keeps the server URL" $ do
    url (makeServerRunConfig Split serverUrl clientUrl) `shouldBe` serverUrl
  where
    clientUrl = Local {port = 3000, path = Just [absdirP|/app/|]}
    serverUrl = Local {port = 3001, path = Nothing}
