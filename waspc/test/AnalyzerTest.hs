{-# LANGUAGE TypeApplications #-}

module AnalyzerTest where

import Analyzer.TestUtil (ctx)
import Data.Either (isRight)
import Data.List (intercalate)
import Test.Tasty.Hspec
import Wasp.Analyzer
import Wasp.Analyzer.Parser (Ctx)
import qualified Wasp.Analyzer.TypeChecker as TC
import Wasp.AppSpec.Action (Action)
import qualified Wasp.AppSpec.Action as Action
import Wasp.AppSpec.App (App)
import qualified Wasp.AppSpec.App as App
import qualified Wasp.AppSpec.App.Auth as Auth
import qualified Wasp.AppSpec.App.Db as Db
import qualified Wasp.AppSpec.App.Dependency as Dependency
import qualified Wasp.AppSpec.App.Server as Server
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
import qualified Wasp.Psl.Ast.Model as PslModel

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
                "  },",
                "  dependencies: [",
                "    { name: \"redux\", version: \"^4.0.5\" }",
                "  ],",
                "  server: {",
                "    setupFn: import { setupServer } from \"@ext/bar.js\"",
                "  },",
                "  db: {",
                "    system: PostgreSQL",
                "  }",
                "}",
                "",
                "entity User {=psl",
                "  description String",
                "psl=}",
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
                "}",
                "",
                "action updateUser {",
                "  fn: import { updateUser } from \"@ext/foo.js\",",
                "  entities: [User],",
                "  auth: true",
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
                          },
                    App.dependencies =
                      Just
                        [ Dependency.Dependency {Dependency.name = "redux", Dependency.version = "^4.0.5"}
                        ],
                    App.server =
                      Just
                        Server.Server
                          { Server.setupFn = Just $ ExtImport (ExtImportField "setupServer") "@ext/bar.js"
                          },
                    App.db = Just Db.Db {Db.system = Just Db.PostgreSQL}
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
                Entity.makeEntity $
                  PslModel.Body
                    [ PslModel.ElementField $
                        PslModel.Field
                          { PslModel._name = "description",
                            PslModel._type = PslModel.String,
                            PslModel._typeModifiers = [],
                            PslModel._attrs = []
                          }
                    ]
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
                    Query.entities = Just [Ref "User"],
                    Query.auth = Nothing
                  }
              )
            ]
      takeDecls @Query <$> decls `shouldBe` Right expectedQueries

      let expectedAction =
            [ ( "updateUser",
                Action.Action
                  { Action.fn = ExtImport (ExtImportField "updateUser") "@ext/foo.js",
                    Action.entities = Just [Ref "User"],
                    Action.auth = Just True
                  }
              )
            ]
      takeDecls @Action <$> decls `shouldBe` Right expectedAction

    it "Returns a type error if unexisting declaration is referenced" $ do
      let source =
            unlines
              [ "route HomeRoute { path: \"/\", page: NonExistentPage }"
              ]
      takeDecls @Route <$> analyze source
        `shouldBe` Left (TypeError $ TC.mkTypeError (ctx (1, 36) (1, 50)) $ TC.UndefinedIdentifier "NonExistentPage")

    it "Returns a type error if referenced declaration is of wrong type" $ do
      let source =
            unlines
              [ "route HomeRoute { path: \"/\",  page: HomeRoute }"
              ]
      analyze source
        `errorMessageShouldBe` ( ctx (1, 37) (1, 45),
                                 intercalate
                                   "\n"
                                   [ "Type error:",
                                     "  For dictionary field 'page':",
                                     "    Expected type: page (declaration type)",
                                     "    Actual type:   route (declaration type)"
                                   ]
                               )

    it "Works when referenced declaration is declared after the reference." $ do
      let source =
            unlines
              [ "route HomeRoute { path: \"/\",  page: HomePage }",
                "page HomePage { component: import Home from \"@ext/HomePage.js\" }"
              ]
      isRight (analyze source) `shouldBe` True

    describe "Returns correct error message" $ do
      it "For nested unexpected type error" $ do
        let source =
              unlines
                [ "app MyApp {",
                  "  title: \"My app\",",
                  "  dependencies: [",
                  "    { name: \"bar\", version: 13 },",
                  "    { name: \"foo\", version: 14 }",
                  "  ]",
                  "}"
                ]
        analyze source
          `errorMessageShouldBe` ( ctx (4, 29) (4, 30),
                                   intercalate
                                     "\n"
                                     [ "Type error:",
                                       "  For dictionary field 'dependencies':",
                                       "    For list element:",
                                       "      For dictionary field 'version':",
                                       "        Expected type: string",
                                       "        Actual type:   number"
                                     ]
                                 )

      it "For nested unification type error" $ do
        let source =
              unlines
                [ "app MyApp {",
                  "  title: \"My app\",",
                  "  dependencies: [",
                  "    { name: \"bar\", version: 13 },",
                  "    { name: \"foo\", version: \"1.2.3\" }",
                  "  ]",
                  "}"
                ]
        analyze source
          `errorMessageShouldBe` ( ctx (5, 29) (5, 35),
                                   intercalate
                                     "\n"
                                     [ "Type error:",
                                       "  For dictionary field 'version':",
                                       "    Can't mix the following types:",
                                       "     - number",
                                       "     - string"
                                     ]
                                 )

      it "For redundant dictionary field" $ do
        let source =
              unlines
                [ "app MyApp {",
                  "  ttle: \"My app\",",
                  "}"
                ]
        analyze source
          `errorMessageShouldBe` ( ctx (1, 11) (3, 1),
                                   intercalate
                                     "\n"
                                     [ "Type error:",
                                       "  Unexpected dictionary field 'ttle'"
                                     ]
                                 )

isAnalyzerOutputTypeError :: Either AnalyzeError a -> Bool
isAnalyzerOutputTypeError (Left (TypeError _)) = True
isAnalyzerOutputTypeError _ = False

errorMessageShouldBe :: Either AnalyzeError a -> (Ctx, String) -> Expectation
errorMessageShouldBe analyzeResult (c, msg) = case analyzeResult of
  Right _ -> error "Test failed: expected AnalyzerError."
  Left e -> getErrorMessageAndCtx e `shouldBe` (msg, c)
