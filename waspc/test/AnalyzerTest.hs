{-# LANGUAGE TypeApplications #-}

module AnalyzerTest where

import Analyzer.TestUtil (ctx)
import qualified Data.Aeson as Aeson
import Data.Either (isRight)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import NeatInterpolation (trimming)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import qualified Util.Prisma as Util
import Wasp.Analyzer
import Wasp.Analyzer.Parser (Ctx)
import qualified Wasp.Analyzer.TypeChecker as TC
import qualified Wasp.AppSpec.Action as Action
import qualified Wasp.AppSpec.App as App
import qualified Wasp.AppSpec.App.Auth as Auth
import qualified Wasp.AppSpec.App.Client as Client
import qualified Wasp.AppSpec.App.Db as Db
import qualified Wasp.AppSpec.App.EmailSender as EmailSender
import qualified Wasp.AppSpec.App.Server as Server
import qualified Wasp.AppSpec.App.Wasp as Wasp
import Wasp.AppSpec.Core.Ref (Ref (..))
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport (..), ExtImportName (..))
import qualified Wasp.AppSpec.JSON as JSON
import qualified Wasp.AppSpec.Job as Job
import qualified Wasp.AppSpec.Page as Page
import qualified Wasp.AppSpec.Query as Query
import Wasp.AppSpec.Route (Route)
import qualified Wasp.AppSpec.Route as Route
import qualified Wasp.Version as WV

spec_Analyzer :: Spec
spec_Analyzer = do
  describe "Analyzer" $ do
    let prismaSchema =
          Util.getPrismaSchema
            [trimming|
              model User {
                description String
              }
            |]

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
                "    methods: {",
                "      usernameAndPassword: {",
                "        userSignupFields: import { getUserFields } from \"@src/auth/signup.js\",",
                "      }",
                "    },",
                "    onAuthFailedRedirectTo: \"/\",",
                "  },",
                "  server: {",
                "    setupFn: import { setupServer } from \"@src/bar.js\"",
                "  },",
                "  client: {",
                "    rootComponent: import { App } from \"@src/App.jsx\",",
                "    setupFn: import { setupClient } from \"@src/baz.js\",",
                "    baseDir: \"/\"",
                "  },",
                "  db: {",
                "    seeds: [ import { devSeedSimple } from \"@src/dbSeeds.js\" ],",
                "    prismaSetupFn: import { setUpPrisma } from \"@src/setUpPrisma.js\",",
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
                "page HomePage {",
                "  component: import Home from \"@src/pages/Main\"",
                "}",
                "",
                "page ProfilePage {",
                "  component: import { profilePage } from \"@src/pages/Profile\",",
                "  authRequired: true",
                "}",
                "",
                "route HomeRoute { path: \"/\", to: HomePage }",
                "",
                "query getUsers {",
                "  fn: import { getAllUsers } from \"@src/foo.js\",",
                "  entities: [User]",
                "}",
                "",
                "action updateUser {",
                "  fn: import { updateUser } from \"@src/foo.js\",",
                "  entities: [User],",
                "  auth: true",
                "}",
                "",
                "job BackgroundJob {",
                "  executor: PgBoss,",
                "  perform: {",
                "    fn: import { backgroundJob } from \"@src/jobs/baz.js\",",
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

      let decls = analyze prismaSchema source

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
                            Auth.methods =
                              Auth.AuthMethods
                                { Auth.usernameAndPassword =
                                    Just
                                      Auth.UsernameAndPasswordConfig
                                        { Auth.userSignupFields = Just $ ExtImport (ExtImportField "getUserFields") (fromJust $ SP.parseRelFileP "auth/signup.js")
                                        },
                                  Auth.slack = Nothing,
                                  Auth.discord = Nothing,
                                  Auth.google = Nothing,
                                  Auth.keycloak = Nothing,
                                  Auth.gitHub = Nothing,
                                  Auth.email = Nothing
                                },
                            Auth.onAuthFailedRedirectTo = "/",
                            Auth.onAuthSucceededRedirectTo = Nothing,
                            Auth.onBeforeSignup = Nothing,
                            Auth.onAfterSignup = Nothing,
                            Auth.onAfterEmailVerified = Nothing,
                            Auth.onBeforeOAuthRedirect = Nothing,
                            Auth.onBeforeLogin = Nothing,
                            Auth.onAfterLogin = Nothing
                          },
                    App.server =
                      Just
                        Server.Server
                          { Server.setupFn =
                              Just $
                                ExtImport
                                  (ExtImportField "setupServer")
                                  (fromJust $ SP.parseRelFileP "bar.js"),
                            Server.middlewareConfigFn = Nothing,
                            Server.envValidationSchema = Nothing
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
                            Client.baseDir = Just "/",
                            Client.envValidationSchema = Nothing
                          },
                    App.db =
                      Just
                        Db.Db
                          { Db.seeds =
                              Just
                                [ ExtImport
                                    (ExtImportField "devSeedSimple")
                                    (fromJust $ SP.parseRelFileP "dbSeeds.js")
                                ],
                            Db.prismaSetupFn =
                              Just $
                                ExtImport
                                  (ExtImportField "setUpPrisma")
                                  (fromJust $ SP.parseRelFileP "setUpPrisma.js")
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
                    { Job.pgBoss = JSON.JSON <$> Aeson.decode "{\"retryLimit\":1}"
                    }
              )
      let jobSchedule =
            Job.Schedule
              "*/5 * * * *"
              (JSON.JSON <$> Aeson.decode "{\"job\":\"args\"}")
              ( Just $
                  Job.ExecutorOptions
                    { Job.pgBoss = JSON.JSON <$> Aeson.decode "{\"retryLimit\":0}"
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
      takeDecls @Route <$> analyze prismaSchema source
        `shouldBe` Left [TypeError $ TC.mkTypeError (ctx (1, 34) (1, 48)) $ TC.UndefinedIdentifier "NonExistentPage"]

    it "Returns a type error if referenced declaration is of wrong type" $ do
      let source =
            unlines
              [ "route HomeRoute { path: \"/\",  to: HomeRoute }"
              ]
      analyze prismaSchema source
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
                "page HomePage { component: import Home from \"@src/HomePage.js\" }"
              ]
      isRight (analyze prismaSchema source) `shouldBe` True

    describe "Returns correct error message" $ do
      it "For nested unexpected type error" $ do
        let source =
              unlines
                [ "app MyApp {",
                  "  title: \"My app\",",
                  "  db: {",
                  "    seeds: [ (\"foo\", \"bar\") ],",
                  "  }",
                  "}"
                ]
        analyze prismaSchema source
          `errorMessageShouldBe` ( ctx (4, 14) (4, 27),
                                   intercalate
                                     "\n"
                                     [ "Type error:",
                                       "  Expected type: external import",
                                       "  Actual type:   (string, string)",
                                       "",
                                       "  -> For dictionary field 'db':",
                                       "    -> For dictionary field 'seeds':",
                                       "      -> In list"
                                     ]
                                 )

      it "For nested unification type error" $ do
        let source =
              unlines
                [ "app MyApp {",
                  "  title: \"My app\",",
                  "  db: {",
                  "    seeds: [ 42, \"foo\" ],",
                  "  }",
                  "}"
                ]
        analyze prismaSchema source
          `errorMessageShouldBe` ( ctx (4, 18) (4, 22),
                                   intercalate
                                     "\n"
                                     [ "Type error:",
                                       "  Can't mix the following types:",
                                       "   - number",
                                       "   - string"
                                     ]
                                 )

      it "For redundant dictionary field" $ do
        let source =
              unlines
                [ "app MyApp {",
                  "  ttle: \"My app\",",
                  "}"
                ]
        analyze prismaSchema source
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
