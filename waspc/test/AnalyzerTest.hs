{-# LANGUAGE TypeApplications #-}

module AnalyzerTest where

import AST (App (..), AuthMethod (..), Page (..))
import AST.Core.Ref (Ref (..))
import Analyzer
import Test.Tasty.Hspec

spec_Analyzer :: Spec
spec_Analyzer = do
  describe "Analyzer" $ do
    it "Analyzes a well-typed example" $ do
      let source =
            unlines
              [ "page HomePage { content: \"Hello world\" }",
                "app Todo {",
                "  title: \"Todo App\",",
                "  authMethod: EmailAndPassword,",
                "  defaultPage: HomePage",
                "}"
              ]
      let decls = analyze source
      let expectedApps = [("Todo", App {title = "Todo App", authMethod = EmailAndPassword, defaultPage = Ref "HomePage"})]
      takeDecls @App <$> decls `shouldBe` Right expectedApps
      let expectedPages = [("HomePage", Page {content = "Hello world"})]
      takeDecls @Page <$> decls `shouldBe` Right expectedPages
