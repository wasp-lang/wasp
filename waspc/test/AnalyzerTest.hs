{-# LANGUAGE TypeApplications #-}

module AnalyzerTest where

import Data.Either (isRight)
import Test.Tasty.Hspec
import Wasp.Analyzer
import qualified Wasp.Analyzer.TypeChecker as TC
import Wasp.AppSpec.App (App)
import qualified Wasp.AppSpec.App as App
import qualified Wasp.AppSpec.App.Auth as Auth
import Wasp.AppSpec.Core.Ref (Ref (..))
import Wasp.AppSpec.Entity (Entity)
import qualified Wasp.AppSpec.Entity as Entity
import Wasp.AppSpec.Page (Page)
import qualified Wasp.AppSpec.Page as Page

spec_Analyzer :: Spec
spec_Analyzer = do
  describe "Analyzer" $ do
    it "Analyzes a well-typed example" $ do
      let source =
            unlines
              [ "app Todo {",
                "  title: \"Todo App\",",
                "  head: [\"foo\", \"bar\"],",
                "  auth: {",
                "    userEntity: User,",
                "    methods: [EmailAndPassword],",
                "  }",
                "}",
                "",
                "entity User {=psl test psl=}",
                "",
                "page HomePage { content: \"Hello world\" }"
              ]
      let decls = analyze source
      let expectedApps =
            [ ( "Todo",
                App.App
                  { App.title = "Todo App",
                    App.head = Just ["foo", "bar"],
                    App.auth =
                      Just
                        Auth.Auth
                          { Auth.userEntity = Ref "User" :: Ref Entity,
                            Auth.methods = [Auth.EmailAndPassword],
                            Auth.onAuthFailedRedirectTo = Nothing
                          }
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
      let expectedEntities =
            [ ( "User",
                Entity.Entity (Entity.PSL " test ")
              )
            ]
      takeDecls @Entity <$> decls `shouldBe` Right expectedEntities

    -- TODO: Rewrite this to use Route once we add it since that will result in simpler test case.
    it "Returns a type error if unexisting declaration is referenced" $ do
      let source =
            unlines
              [ "app Todo {",
                "  title: \"Todo App\",",
                "  auth: { userEntity: NonExistentEntity, methods: [EmailAndPassword] }",
                "}"
              ]
      takeDecls @App <$> analyze source `shouldBe` Left (TypeError $ TC.UndefinedIdentifier "NonExistentEntity")

    -- TODO: Rewrite this to use Route once we add it since that will result in simpler test case.
    it "Returns a type error if referenced declaration is of wrong type" $ do
      let source =
            unlines
              [ "app Todo {",
                "  title: \"Todo App\",",
                "  auth: { userEntity: Todo, methods: [EmailAndPassword] }",
                "}"
              ]
      takeDecls @App <$> analyze source `shouldSatisfy` isAnalyzerOutputTypeError

    -- TODO: Rewrite this to use Route once we add it since that will result in simpler test case.
    it "Works when referenced declaration is declared after the reference." $ do
      let source =
            unlines
              [ "app Todo {",
                "  title: \"Todo App\",",
                "  auth: { userEntity: User, methods: [EmailAndPassword] }",
                "}",
                "entity User {=psl test psl=}"
              ]
      isRight (analyze source) `shouldBe` True

isAnalyzerOutputTypeError :: Either AnalyzeError a -> Bool
isAnalyzerOutputTypeError (Left (TypeError _)) = True
isAnalyzerOutputTypeError _ = False
