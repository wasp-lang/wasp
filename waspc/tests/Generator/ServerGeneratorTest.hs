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

spec_genDotEnv :: Spec
spec_genDotEnv = do
  describe "genDotEnv" $ do
    it "writes the user-provided server env vars" $ do
      genDotEnvContent basicAppSpec {AS.devEnvVarsServer = [("FOO", "bar"), ("BAZ", "qux")]}
        `shouldBe` Just "FOO=bar\nBAZ=qux"

    it "writes an empty .env when there are no user-provided server env vars" $ do
      genDotEnvContent basicAppSpec {AS.devEnvVarsServer = []}
        `shouldBe` Just ""

    it "generates no .env for production builds" $ do
      case runGenerator $ genDotEnv basicAppSpec {AS.buildType = BuildType.Production} of
        (_, Right drafts) -> length drafts `shouldBe` 0
        (_, Left _) -> expectationFailure "genDotEnv failed"
  where
    genDotEnvContent :: AS.AppSpec -> Maybe T.Text
    genDotEnvContent spec = case runGenerator $ genDotEnv spec of
      (_, Right [FileDraftTextFd draft]) -> Just draft._content
      _ -> Nothing
