module Generator.ServerGeneratorTest where

import qualified Data.Text as T
import Fixtures.AppSpec (basicAppSpec)
import Test.Hspec
import qualified Wasp.AppSpec as AS
import Wasp.Generator.FileDraft (FileDraft (FileDraftTextFd))
import Wasp.Generator.FileDraft.TextFileDraft (TextFileDraft (_content))
import Wasp.Generator.Monad (runGenerator)
import Wasp.Generator.ServerGenerator (genDotEnv)
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Db (databaseUrlEnvVarName)

spec_genDotEnv :: Spec
spec_genDotEnv = do
  describe "genDotEnv" $ do
    it ("does not generate a managed " <> databaseUrlEnvVarName) $ do
      genDotEnvContent basicAppSpec `shouldBe` Just ""

    it ("keeps the user-provided " <> databaseUrlEnvVarName) $ do
      genDotEnvContent basicAppSpec {AS.devEnvVarsServer = [(databaseUrlEnvVarName, userDbUrl)]}
        `shouldBe` Just (T.pack $ databaseUrlEnvVarName <> "=" <> userDbUrl)

    it "generates no .env for production builds" $ do
      case runGenerator $ genDotEnv basicAppSpec {AS.buildType = BuildType.Production} of
        (_, Right drafts) -> length drafts `shouldBe` 0
        (_, Left _) -> expectationFailure "genDotEnv failed"
  where
    userDbUrl = "postgresql://userUser:userPass@localhost:9999/userDb"

    genDotEnvContent :: AS.AppSpec -> Maybe T.Text
    genDotEnvContent spec = case runGenerator $ genDotEnv spec of
      (_, Right [FileDraftTextFd draft]) -> Just draft._content
      _ -> Nothing
