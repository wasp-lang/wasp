module Wasp.Cli.Command.CreateNewProject.ProjectDescriptionTest where

import Data.Either (isLeft)
import Test.Hspec
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectAppName (..),
    parseWaspProjectNameIntoAppName,
  )

spec_parseWaspProjectNameIntoAppName :: Spec
spec_parseWaspProjectNameIntoAppName = do
  it "accepts standard kebab-case and camelCase names" $ do
    appName "my-app" `shouldBe` Right "myApp"
    appName "myApp" `shouldBe` Right "myApp"
    appName "app-1" `shouldBe` Right "app1"
    appName "_app" `shouldBe` Right "_app"

  it "rejects names that only become valid after kebab-case conversion" $ do
    isLeft (parseWaspProjectNameIntoAppName "-app") `shouldBe` True
    isLeft (parseWaspProjectNameIntoAppName "--app") `shouldBe` True

  it "rejects names with characters outside the documented rules" $ do
    isLeft (parseWaspProjectNameIntoAppName "app'") `shouldBe` True
    isLeft (parseWaspProjectNameIntoAppName "app!") `shouldBe` True
    isLeft (parseWaspProjectNameIntoAppName "my app") `shouldBe` True

  it "rejects Wasp keywords" $ do
    isLeft (parseWaspProjectNameIntoAppName "import") `shouldBe` True
  where
    appName name = fmap (\(NewProjectAppName n) -> n) $ parseWaspProjectNameIntoAppName name
