{-# LANGUAGE TypeApplications #-}

module AnalyzerTest where

import Analyzer.TestUtil (ctx)
import qualified Data.Aeson as Aeson
import Data.Either (isRight)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import Wasp.Analyzer
import Wasp.Analyzer.Parser (Ctx)
import qualified Wasp.Analyzer.TypeChecker as TC
import qualified Wasp.AppSpec.Action as Action
import qualified Wasp.AppSpec.App as App
import qualified Wasp.AppSpec.App.Auth as Auth
import qualified Wasp.AppSpec.App.Client as Client
import qualified Wasp.AppSpec.App.Db as Db
import qualified Wasp.AppSpec.App.Dependency as Dependency
import qualified Wasp.AppSpec.App.EmailSender as EmailSender
import qualified Wasp.AppSpec.App.Server as Server
import qualified Wasp.AppSpec.App.Wasp as Wasp
import Wasp.AppSpec.Core.Ref (Ref (..))
import Wasp.AppSpec.Entity (Entity)
import qualified Wasp.AppSpec.Entity as Entity
import Wasp.AppSpec.ExtImport (ExtImport (..), ExtImportName (..))
import qualified Wasp.AppSpec.JSON as JSON
import qualified Wasp.AppSpec.Job as Job
import qualified Wasp.AppSpec.Page as Page
import qualified Wasp.AppSpec.Query as Query
import Wasp.AppSpec.Route (Route)
import qualified Wasp.AppSpec.Route as Route
import qualified Wasp.Psl.Ast.Model as PslModel
import qualified Wasp.Version as WV

spec_Analyzer :: Spec
spec_Analyzer = do
  describe "Analyzer" $ do
    it "Analyzes a well-typed example" $ do
      let source =
            unlines
              [ "app Todo {",
                "  wasp: {",
                "    version: \"^" ++ show WV.waspVersion ++ "\",",
                "  },",
                "  title: \"Todo App\",",
                "  head: [\"foo\", \"bar\"],",
                "  auth: {",
                "    userEntity: User,",
                "    methods: { usernameAndPassword: {} },",
                "    onAuthFailedRedirectTo: \"/\",",
                "    signup: {",
                "      additionalFields: import { fields } from \"@server/auth/signup.js\",",
                "    },",
                "  },",
                "  dependencies: [",
                "    (\"redux\", \"^4.0.5\")",
                "  ],",
                "  server: {",
                "    setupFn: import { setupServer } from \"@server/bar.js\"",
                "  },",
                "  client: {",
                "    rootComponent: import { App } from \"@client/App.jsx\",",
                "    setupFn: import { setupClient } from \"@client/baz.js\",",
                "    baseDir: \"/\"",
                "  },",
                "  db: {",
                "    system: PostgreSQL,",
                "    seeds: [ import { devSeedSimple } from \"@server/dbSeeds.js\" ],",
                "    prisma: {",
                "      clientPreviewFeatures: [\"extendedWhereUnique\"],",
                "      dbExtensions: [{ name: \"pg_trgm\", version: \"1.0.0\" }]",
                "    }",
                "  },",
                "  emailSender: {",
                "    provider: SendGrid,",
                "    defaultFrom: {",
                "      email: \"test@test.com\",",
                "      name: \"Test\"",
                "    }",
                "  }",
                "}",
                "",
                "entity User {=psl",
                "  description String",
                "psl=}",
                "",
                "page HomePage {",
                "  component: import Home from \"@client/pages/Main\"",
                "}",
                "",
                "page ProfilePage {",
                "  component: import { profilePage } from \"@client/pages/Profile\",",
                "  authRequired: true",
                "}",
                "",
                "route HomeRoute { path: \"/\", to: HomePage }",
                "",
                "query getUsers {",
                "  fn: import { getAllUsers } from \"@server/foo.js\",",
                "  entities: [User]",
                "}",
                "",
                "action updateUser {",
                "  fn: import { updateUser } from \"@server/foo.js\",",
                "  entities: [User],",
                "  auth: true",
                "}",
                "",
                "job BackgroundJob {",
                "  executor: PgBoss,",
                "  perform: {",
                "    fn: import { backgroundJob } from \"@server/jobs/baz.js\",",
                "    executorOptions: {",
                "      pgBoss: {=json { \"retryLimit\": 1 } json=}",
                "    }",
                "  },",
                "  schedule: {",
                "    cron: \"*/5 * * * *\",",
                "    args: {=json { \"job\": \"args\" } json=},",
                "    executorOptions: {",
                "      pgBoss: {=json { \"retryLimit\": 0 } json=}",
                "    }",
                "  }",
                "}"
              ]

      let decls = analyze source

      let expectedApps =
            [ ( "Todo",
                App.App
                  { App.wasp = Wasp.Wasp {Wasp.version = "^" ++ show WV.waspVersion},
                    App.title = "Todo App",
                    App.head = Just ["foo", "bar"],
                    App.auth =
                      Just
                        Auth.Auth
                          { Auth.userEntity = Ref "User" :: Ref Entity,
                            Auth.externalAuthEntity = Nothing,
                            Auth.signup =
                              Just $
                                Auth.SignupOptions
                                  { Auth.additionalFields =
                                      Just $ ExtImport (ExtImportField "fields") (fromJust $ SP.parseRelFileP "auth/signup.js")
                                  },
                            Auth.methods =
                              Auth.AuthMethods
                                { Auth.usernameAndPassword = Just Auth.usernameAndPasswordConfig,
                                  Auth.google = Nothing,
                                  Auth.gitHub = Nothing,
                                  Auth.email = Nothing
                                },
                            Auth.onAuthFailedRedirectTo = "/",
                            Auth.onAuthSucceededRedirectTo = Nothing
                          },
                    App.dependencies =
                      Just
                        [ Dependency.Dependency {Dependency.name = "redux", Dependency.version = "^4.0.5"}
                        ],
                    App.server =
                      Just
                        Server.Server
                          { Server.setupFn =
                              Just $
                                ExtImport
                                  (ExtImportField "setupServer")
                                  (fromJust $ SP.parseRelFileP "bar.js"),
                            Server.middlewareConfigFn = Nothing
                          },
                    App.client =
                      Just
                        Client.Client
                          { Client.setupFn =
                              Just $
                                ExtImport (ExtImportField "setupClient") (fromJust $ SP.parseRelFileP "baz.js"),
                            Client.rootComponent =
                              Just $
                                ExtImport (ExtImportField "App") (fromJust $ SP.parseRelFileP "App.jsx"),
                            Client.baseDir = Just "/"
                          },
                    App.db =
                      Just
                        Db.Db
                          { Db.system = Just Db.PostgreSQL,
                            Db.seeds =
                              Just
                                [ ExtImport
                                    (ExtImportField "devSeedSimple")
                                    (fromJust $ SP.parseRelFileP "dbSeeds.js")
                                ],
                            Db.prisma =
                              Just
                                Db.PrismaOptions
                                  { clientPreviewFeatures = Just ["extendedWhereUnique"],
                                    dbExtensions = Just [Db.PrismaDbExtension {Db.name = "pg_trgm", Db.version = Just "1.0.0", Db.map = Nothing, Db.schema = Nothing}]
                                  }
                          },
                    App.emailSender =
                      Just
                        EmailSender.EmailSender
                          { EmailSender.provider = EmailSender.SendGrid,
                            EmailSender.defaultFrom =
                              Just
                                EmailSender.EmailFromField
                                  { EmailSender.email = "test@test.com",
                                    EmailSender.name = Just "Test"
                                  }
                          },
                    App.webSocket = Nothing
                  }
              )
            ]
      takeDecls <$> decls `shouldBe` Right expectedApps

      let expectedPages =
            [ ( "HomePage",
                Page.Page
                  { Page.component =
                      ExtImport
                        (ExtImportModule "Home")
                        (fromJust $ SP.parseRelFileP "pages/Main"),
                    Page.authRequired = Nothing
                  }
              ),
              ( "ProfilePage",
                Page.Page
                  { Page.component =
                      ExtImport
                        (ExtImportField "profilePage")
                        (fromJust $ SP.parseRelFileP "pages/Profile"),
                    Page.authRequired = Just True
                  }
              )
            ]
      takeDecls <$> decls `shouldBe` Right expectedPages

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
      takeDecls <$> decls `shouldBe` Right expectedEntities

      let expectedRoutes =
            [ ( "HomeRoute",
                Route.Route {Route.path = "/", Route.to = Ref "HomePage"}
              )
            ]
      takeDecls <$> decls `shouldBe` Right expectedRoutes

      let expectedQueries =
            [ ( "getUsers",
                Query.Query
                  { Query.fn =
                      ExtImport
                        (ExtImportField "getAllUsers")
                        (fromJust $ SP.parseRelFileP "foo.js"),
                    Query.entities = Just [Ref "User"],
                    Query.auth = Nothing
                  }
              )
            ]
      takeDecls <$> decls `shouldBe` Right expectedQueries

      let expectedAction =
            [ ( "updateUser",
                Action.Action
                  { Action.fn =
                      ExtImport
                        (ExtImportField "updateUser")
                        (fromJust $ SP.parseRelFileP "foo.js"),
                    Action.entities = Just [Ref "User"],
                    Action.auth = Just True
                  }
              )
            ]
      takeDecls <$> decls `shouldBe` Right expectedAction

      let jobPerform =
            Job.Perform
              ( ExtImport
                  (ExtImportField "backgroundJob")
                  (fromJust $ SP.parseRelFileP "jobs/baz.js")
              )
              ( Just $
                  Job.ExecutorOptions
                    { Job.pgBoss = JSON.JSON <$> Aeson.decode "{\"retryLimit\":1}",
                      Job.simple = Nothing
                    }
              )
      let jobSchedule =
            Job.Schedule
              "*/5 * * * *"
              (JSON.JSON <$> Aeson.decode "{\"job\":\"args\"}")
              ( Just $
                  Job.ExecutorOptions
                    { Job.pgBoss = JSON.JSON <$> Aeson.decode "{\"retryLimit\":0}",
                      Job.simple = Nothing
                    }
              )
      let expectedJob =
            [ ( "BackgroundJob",
                Job.Job
                  { Job.executor = Job.PgBoss,
                    Job.perform = jobPerform,
                    Job.schedule = Just jobSchedule,
                    Job.entities = Nothing
                  }
              )
            ]
      takeDecls <$> decls `shouldBe` Right expectedJob

    it "Returns a type error if unexisting declaration is referenced" $ do
      let source =
            unlines
              [ "route HomeRoute { path: \"/\", to: NonExistentPage }"
              ]
      takeDecls @Route <$> analyze source
        `shouldBe` Left [TypeError $ TC.mkTypeError (ctx (1, 34) (1, 48)) $ TC.UndefinedIdentifier "NonExistentPage"]

    it "Returns a type error if referenced declaration is of wrong type" $ do
      let source =
            unlines
              [ "route HomeRoute { path: \"/\",  to: HomeRoute }"
              ]
      analyze source
        `errorMessageShouldBe` ( ctx (1, 35) (1, 43),
                                 intercalate
                                   "\n"
                                   [ "Type error:",
                                     "  Expected type: page (declaration type)",
                                     "  Actual type:   route (declaration type)",
                                     "",
                                     "  -> For dictionary field 'to'"
                                   ]
                               )

    it "Works when referenced declaration is declared after the reference." $ do
      let source =
            unlines
              [ "route HomeRoute { path: \"/\",  to: HomePage }",
                "page HomePage { component: import Home from \"@client/HomePage.js\" }"
              ]
      isRight (analyze source) `shouldBe` True

    describe "Returns correct error message" $ do
      it "For nested unexpected type error" $ do
        let source =
              unlines
                [ "app MyApp {",
                  "  title: \"My app\",",
                  "  dependencies: [",
                  "    (\"bar\", 13),",
                  "    (\"foo\", 14)",
                  "  ]",
                  "}"
                ]
        analyze source
          `errorMessageShouldBe` ( ctx (4, 5) (4, 15),
                                   intercalate
                                     "\n"
                                     [ "Type error:",
                                       "  Expected type: (string, string)",
                                       "  Actual type:   (string, number)",
                                       "",
                                       "  -> For dictionary field 'dependencies':",
                                       "    -> In list"
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
                                       "  Can't mix the following types:",
                                       "   - number",
                                       "   - string",
                                       "",
                                       "  -> For dictionary field 'version'"
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

errorMessageShouldBe :: Either [AnalyzeError] a -> (Ctx, String) -> Expectation
errorMessageShouldBe analyzeResult (c, msg) = case analyzeResult of
  Right _ -> error "Test failed: expected AnalyzerError."
  Left [e] -> getErrorMessageAndCtx e `shouldBe` (msg, c)
  Left errs -> length errs `shouldBe` 1
