module Wasp.Cli.Command.CreateNewProject.ProjectDescriptionTest where

import Data.Either (isLeft)
import Test.Hspec
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName (..), parseWaspProjectNameIntoAppName)

spec_parseWaspProjectNameIntoAppName :: Spec
spec_parseWaspProjectNameIntoAppName = do
  describe "parseWaspProjectNameIntoAppName" $ do
    it "converts a kebab-case name to a camelCase app name" $ do
      parseWaspProjectNameIntoAppName "my-app" `shouldParseToAppName` "myApp"

    it "accepts a name starting with an underscore" $ do
      parseWaspProjectNameIntoAppName "_app" `shouldParseToAppName` "_app"

    -- Regression tests for https://github.com/wasp-lang/wasp/issues/4763:
    -- these names used to pass validation because the check ran on the
    -- converted app name ("-app" becomes "app"), while the project
    -- directory is created from the raw name.
    it "rejects a name starting with a dash" $ do
      parseWaspProjectNameIntoAppName "-app" `shouldSatisfy` isLeft

    it "rejects a name with a trailing prime" $ do
      parseWaspProjectNameIntoAppName "app'" `shouldSatisfy` isLeft

    it "rejects a Wasp keyword" $ do
      parseWaspProjectNameIntoAppName "import" `shouldSatisfy` isLeft

shouldParseToAppName :: Either String NewProjectAppName -> String -> Expectation
shouldParseToAppName result expectedAppName = case result of
  Right (NewProjectAppName appName) -> appName `shouldBe` expectedAppName
  Left err ->
    expectationFailure $
      "Expected app name " ++ show expectedAppName ++ ", but got error: " ++ err
