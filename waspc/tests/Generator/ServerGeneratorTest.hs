module Generator.ServerGeneratorTest where

import qualified Data.Text as T
import Fixtures.AppSpec (basicAppSpec)
import Test.Hspec
import qualified Wasp.AppSpec as AS
import Wasp.DevEnvironment (DevEnvironment (..))
import Wasp.Generator.FileDraft (FileDraft (FileDraftTextFd))
import Wasp.Generator.FileDraft.TextFileDraft (TextFileDraft (_content))
import Wasp.Generator.Monad (runGenerator)
import Wasp.Generator.ServerGenerator (genDotEnv)
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Db (databaseUrlEnvVarName)

spec_genDotEnv :: Spec
spec_genDotEnv = describe "genDotEnv" $ do
  it "writes the managed database URL when known" $
    genDotEnvContent basicAppSpec (DevEnvironment $ Just devDbUrl)
      `shouldBe` Just (T.pack $ databaseUrlEnvVarName <> "=" <> devDbUrl)

  it "omits the database URL when unknown" $
    genDotEnvContent basicAppSpec (DevEnvironment Nothing) `shouldBe` Just ""

  it "prefers the user-provided database URL" $
    genDotEnvContent basicAppSpec {AS.devEnvVarsServer = [(databaseUrlEnvVarName, userDbUrl)]} (DevEnvironment $ Just devDbUrl)
      `shouldBe` Just (T.pack $ databaseUrlEnvVarName <> "=" <> userDbUrl)

  it "generates no .env for production builds" $
    case runGenerator $ genDotEnv basicAppSpec {AS.buildType = BuildType.Production} (DevEnvironment $ Just devDbUrl) of
      (_, Right drafts) -> length drafts `shouldBe` 0
      (_, Left _) -> expectationFailure "genDotEnv failed"
  where
    devDbUrl = "postgresql://devUser:devPass@localhost:5432/devDb"
    userDbUrl = "postgresql://userUser:userPass@localhost:9999/userDb"

    genDotEnvContent :: AS.AppSpec -> DevEnvironment -> Maybe T.Text
    genDotEnvContent spec devEnvironment = case runGenerator $ genDotEnv spec devEnvironment of
      (_, Right [FileDraftTextFd draft]) -> Just draft._content
      _ -> Nothing
