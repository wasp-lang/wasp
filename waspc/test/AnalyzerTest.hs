{-# LANGUAGE TypeApplications #-}

module AnalyzerTest where

import AST (App (..), AuthMethod (..), Page (..))
import AST.Core.Ref (Ref (..))
import Analyzer
import qualified Analyzer.Evaluator as E
import qualified Analyzer.Evaluator.EvaluationError as E.Error
import qualified Analyzer.Type as T
import qualified Analyzer.TypeChecker as TC
import qualified Analyzer.TypeChecker.TypeError as T.Error
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

    it "Returns a type error if unexisting declaration is referenced" $ do
      let source =
            unlines
              [ "page HomePage { content: \"Hello world\" }",
                "app Todo {",
                "  title: \"Todo App\",",
                "  authMethod: EmailAndPassword,",
                "  defaultPage: NonExistentPage",
                "}"
              ]
      takeDecls @App <$> analyze source `shouldBe` Left (TypeError $ TC.UndefinedIdentifier "NonExistentPage")

    it "Returns a type error if referenced declaration is of wrong type" $ do
      let source =
            unlines
              [ "app Todo {",
                "  title: \"Todo App\",",
                "  authMethod: EmailAndPassword,",
                "  defaultPage: Todo",
                "}"
              ]
      takeDecls @App <$> analyze source `shouldSatisfy` isAnalyzerOutputTypeError

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

isAnalyzerOutputTypeError :: Either AnalyzeError a -> Bool
isAnalyzerOutputTypeError (Left (TypeError _)) = True
isAnalyzerOutputTypeError _ = False
