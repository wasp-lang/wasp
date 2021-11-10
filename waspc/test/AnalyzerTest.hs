{-# LANGUAGE TypeApplications #-}

module AnalyzerTest where

import Wasp.Analyzer
import qualified Wasp.Analyzer.TypeChecker as TC
import Wasp.AppSpec.App (App)
import qualified Wasp.AppSpec.App as App
import qualified Wasp.AppSpec.AuthMethod as AuthMethod
import Wasp.AppSpec.Core.Ref (Ref (..))
import Wasp.AppSpec.Page (Page)
import qualified Wasp.AppSpec.Page as Page
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
      let expectedApps =
            [ ( "Todo",
                App.App
                  { App.title = "Todo App",
                    App.authMethod = AuthMethod.EmailAndPassword,
                    App.defaultPage = Ref "HomePage"
                  }
              )
            ]
      takeDecls @App <$> decls `shouldBe` Right expectedApps
      let expectedPages =
            [ ( "HomePage",
                Page.Page
                  { Page.content = "Hello world"
                  }
              )
            ]
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
