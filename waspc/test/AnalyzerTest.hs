{-# LANGUAGE TypeApplications #-}

module AnalyzerTest where

import AST (App (..), AuthMethod (..), Page (..))
import AST.Core.Ref (Ref (..))
import Analyzer
import qualified Analyzer.TypeChecker as T
import Data.Either (isRight)
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
                "  defaultPage: HomePage",
                "}",
                "page HomePage { content: \"Hello world\" }"
              ]
      let decls = analyze source
      let expectedApps = [("Todo", App {title = "Todo App", authMethod = EmailAndPassword, defaultPage = Ref "HomePage"})]
      takeDecls @App <$> decls `shouldBe` Right expectedApps
      let expectedPages = [("HomePage", Page {content = "Hello world"})]
      takeDecls @Page <$> decls `shouldBe` Right expectedPages

    it "Returns an error if unexisting declaration is referenced" $ do
      let source =
            unlines
              [ "page HomePage { content: \"Hello world\" }",
                "app Todo {",
                "  title: \"Todo App\",",
                "  authMethod: EmailAndPassword,",
                "  defaultPage: NonExistentPage",
                "}"
              ]
      takeDecls @App <$> analyze source `shouldBe` Left (TypeError $ T.UndefinedIdentifier "NonExistentPage")

    it "Works when referenced declaration is declared after the reference." $ do
      let source =
            unlines
              [ "app Todo {",
                "  title: \"Todo App\",",
                "  authMethod: EmailAndPassword,",
                "  defaultPage: HomePage",
                "}",
                "page HomePage { content: \"Hello world\" }"
              ]
      isRight (analyze source) `shouldBe` True
