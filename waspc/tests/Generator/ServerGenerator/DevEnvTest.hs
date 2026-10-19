module Generator.ServerGenerator.DevEnvTest where

import qualified Data.Text as T
import Fixtures.AppSpec (basicAppSpec)
import Test.Hspec
import qualified Wasp.AppSpec as AS
import Wasp.Generator.FileDraft (FileDraft (FileDraftTextFd))
import Wasp.Generator.FileDraft.TextFileDraft (TextFileDraft (_content))
import Wasp.Generator.ServerGenerator.DevEnv (makeDevEnvFileDraft, prepareDevEnv)
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Db (databaseUrlEnvVarName)

spec_devEnv :: Spec
spec_devEnv = describe "development server environment" $ do
  it "writes the managed database URL when known" $
    envContent [] (Just devDbUrl)
      `shouldBe` Just (T.pack $ databaseUrlEnvVarName <> "=" <> devDbUrl)

  it "omits the database URL when unknown" $
    envContent [] Nothing `shouldBe` Just ""

  it "prefers the user-provided database URL" $
    envContent [(databaseUrlEnvVarName, userDbUrl)] (Just devDbUrl)
      `shouldBe` Just (T.pack $ databaseUrlEnvVarName <> "=" <> userDbUrl)

  it "prepares no .env for production builds" $
    length <$> prepareDevEnv basicAppSpec {AS.buildType = BuildType.Production}
      `shouldReturn` 0
  where
    devDbUrl = "postgresql://devUser:devPass@localhost:5432/devDb"
    userDbUrl = "postgresql://userUser:userPass@localhost:9999/userDb"

    envContent :: [(String, String)] -> Maybe String -> Maybe T.Text
    envContent userEnvVars databaseUrl = case makeDevEnvFileDraft userEnvVars databaseUrl of
      FileDraftTextFd draft -> Just draft._content
      _ -> Nothing
