module Wasp.Cli.Command.CreateNewProject.ProjectDescriptionTest where

import Data.Either (isLeft)
import Test.Hspec
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName (..), parseWaspProjectNameIntoAppName)

spec_parseWaspProjectNameIntoAppName :: Spec
spec_parseWaspProjectNameIntoAppName = do
  describe "parseWaspProjectNameIntoAppName" $ do
    describe "valid project names" $ do
      it "accepts a kebab-case name and converts it to a camelCase app name" $ do
        parseWaspProjectNameIntoAppName "my-app" `shouldParseToAppName` "myApp"

      it "accepts a name with multiple dashes" $ do
        parseWaspProjectNameIntoAppName "my-cool-app2" `shouldParseToAppName` "myCoolApp2"

      it "accepts a plain lowercase name" $ do
        parseWaspProjectNameIntoAppName "myapp" `shouldParseToAppName` "myapp"

      it "accepts a name with underscores" $ do
        parseWaspProjectNameIntoAppName "my_app" `shouldParseToAppName` "my_app"

      it "accepts a name starting with an underscore" $ do
        parseWaspProjectNameIntoAppName "_app" `shouldParseToAppName` "_app"

      it "accepts a name containing numbers" $ do
        parseWaspProjectNameIntoAppName "app2" `shouldParseToAppName` "app2"

    describe "invalid project names" $ do
      -- Regression test for https://github.com/wasp-lang/wasp/issues/4763:
      -- these names used to pass validation because the check ran on the
      -- converted app name ("-app" becomes "App"), while the project
      -- directory is created from the raw name.
      it "rejects a name starting with a dash" $ do
        parseWaspProjectNameIntoAppName "-app" `shouldSatisfy` isLeft

      it "rejects a name with a trailing prime" $ do
        parseWaspProjectNameIntoAppName "app'" `shouldSatisfy` isLeft

      it "rejects a name starting with a number" $ do
        parseWaspProjectNameIntoAppName "2app" `shouldSatisfy` isLeft

      it "rejects a name containing a space" $ do
        parseWaspProjectNameIntoAppName "my app" `shouldSatisfy` isLeft

      it "rejects a name containing other special characters" $ do
        parseWaspProjectNameIntoAppName "app!" `shouldSatisfy` isLeft

      it "rejects an empty name" $ do
        parseWaspProjectNameIntoAppName "" `shouldSatisfy` isLeft

      it "rejects a Wasp keyword" $ do
        parseWaspProjectNameIntoAppName "import" `shouldSatisfy` isLeft

      it "explains the name format rules in the error message" $ do
        case parseWaspProjectNameIntoAppName "-app" of
          Left err -> err `shouldContain` "must start with a letter or an underscore"
          Right _ -> expectationFailure "Expected an error for \"-app\", but the name was accepted"

shouldParseToAppName :: Either String NewProjectAppName -> String -> Expectation
shouldParseToAppName result expectedAppName = case result of
  Right (NewProjectAppName appName) -> appName `shouldBe` expectedAppName
  Left err ->
    expectationFailure $
      "Expected app name " ++ show expectedAppName ++ ", but got error: " ++ err
