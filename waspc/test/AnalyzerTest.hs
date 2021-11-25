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
import Wasp.AppSpec.ExtImport (ExtImport (..), ExtImportName (..))
import Wasp.AppSpec.Page (Page)
import qualified Wasp.AppSpec.Page as Page
import Wasp.AppSpec.Query (Query)
import qualified Wasp.AppSpec.Query as Query
import Wasp.AppSpec.Route (Route)
import qualified Wasp.AppSpec.Route as Route

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
                "page HomePage {",
                "  component: import Home from \"@ext/pages/Main\"",
                "}",
                "",
                "page ProfilePage {",
                "  component: import { profilePage } from \"@ext/pages/Profile\",",
                "  authRequired: true",
                "}",
                "",
                "route HomeRoute { path: \"/\", page: HomePage }",
                "",
                "query getUsers {",
                "  fn: import { getAllUsers } from \"@ext/foo.js\",",
                "  entities: [User]",
                "}"
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
                  { Page.component = ExtImport (ExtImportModule "Home") "@ext/pages/Main",
                    Page.authRequired = Nothing
                  }
              ),
              ( "ProfilePage",
                Page.Page
                  { Page.component = ExtImport (ExtImportField "profilePage") "@ext/pages/Profile",
                    Page.authRequired = Just True
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

      let expectedRoutes =
            [ ( "HomeRoute",
                Route.Route {Route.path = "/", Route.page = Ref "HomePage"}
              )
            ]
      takeDecls @Route <$> decls `shouldBe` Right expectedRoutes

      let expectedQueries =
            [ ( "getUsers",
                Query.Query
                  { Query.fn = ExtImport (ExtImportField "getAllUsers") "@ext/foo.js",
                    Query.entities = [Ref "User"]
                  }
              )
            ]
      takeDecls @Query <$> decls `shouldBe` Right expectedQueries

    it "Returns a type error if unexisting declaration is referenced" $ do
      let source =
            unlines
              [ "route HomeRoute { path: \"/\",  page: NonExistentPage }"
              ]
      takeDecls @Route <$> analyze source `shouldBe` Left (TypeError $ TC.UndefinedIdentifier "NonExistentPage")

    it "Returns a type error if referenced declaration is of wrong type" $ do
      let source =
            unlines
              [ "route HomeRoute { path: \"/\",  page: HomeRoute }"
              ]
      takeDecls @Route <$> analyze source `shouldSatisfy` isAnalyzerOutputTypeError

    it "Works when referenced declaration is declared after the reference." $ do
      let source =
            unlines
              [ "route HomeRoute { path: \"/\",  page: HomePage }",
                "page HomePage { component: import Home from \"@ext/HomePage.js\" }"
              ]
      isRight (analyze source) `shouldBe` True

isAnalyzerOutputTypeError :: Either AnalyzeError a -> Bool
isAnalyzerOutputTypeError (Left (TypeError _)) = True
isAnalyzerOutputTypeError _ = False
