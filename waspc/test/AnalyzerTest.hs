module AnalyzerTest where

import Analyzer
import Analyzer.StdTypeDefinitions (App (..), AuthMethod (..))
import Test.Tasty.Hspec

spec_Analyzer :: Spec
spec_Analyzer = do
  describe "Analyzer" $ do
    it "Analyzes a well-typed example" $ do
      let source =
            unlines
              [ "app Todo {",
                "  title: \"Todo App\",",
                "  authMethod: EmailAndPassword,",
                "}"
              ]
      let expectedApps = [("Todo", App {title = "Todo App", authMethod = EmailAndPassword})]
      takeDecls <$> analyze source `shouldBe` Right expectedApps
