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
    it ("writes " <> databaseUrlEnvVarName <> " when the dev database url is known") $ do
      genDotEnvContent basicAppSpec {AS.devDatabaseUrl = Just devDbUrl}
        `shouldBe` Just (T.pack $ databaseUrlEnvVarName <> "=" <> devDbUrl)

    it ("omits " <> databaseUrlEnvVarName <> " when the dev database url is not known") $ do
      genDotEnvContent basicAppSpec {AS.devDatabaseUrl = Nothing}
        `shouldBe` Just ""

    it ("prefers the user-provided " <> databaseUrlEnvVarName <> " over the dev database one") $ do
      genDotEnvContent
        basicAppSpec
          { AS.devDatabaseUrl = Just devDbUrl,
            AS.devEnvVarsServer = [(databaseUrlEnvVarName, userDbUrl)]
          }
        `shouldBe` Just (T.pack $ databaseUrlEnvVarName <> "=" <> userDbUrl)

    it "generates no .env for production builds" $ do
      case runGenerator $ genDotEnv basicAppSpec {AS.buildType = BuildType.Production, AS.devDatabaseUrl = Just devDbUrl} of
        (_, Right drafts) -> length drafts `shouldBe` 0
        (_, Left _) -> expectationFailure "genDotEnv failed"
  where
    devDbUrl = "postgresql://devUser:devPass@localhost:5432/devDb"
    userDbUrl = "postgresql://userUser:userPass@localhost:9999/userDb"

    genDotEnvContent :: AS.AppSpec -> Maybe T.Text
    genDotEnvContent spec = case runGenerator $ genDotEnv spec of
      (_, Right [FileDraftTextFd draft]) -> Just draft._content
      _ -> Nothing
