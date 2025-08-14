{-# LANGUAGE TypeApplications #-}

module AppSpec.ValidTest where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Fixtures (systemSPRoot)
import NeatInterpolation (trimming)
import StrongPath (relfile)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import qualified Util.Prisma as Util
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.ApiNamespace as AS.ApiNamespace
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Auth.EmailVerification as AS.Auth.EmailVerification
import qualified Wasp.AppSpec.App.Auth.PasswordReset as AS.Auth.PasswordReset
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Entity as AS.Entity
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Job as AS.Job
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Query as AS.Query
import qualified Wasp.AppSpec.Route as AS.Route
import qualified Wasp.AppSpec.Valid as ASV
import qualified Wasp.ExternalConfig.Npm.PackageJson as Npm.PackageJson
import qualified Wasp.ExternalConfig.TsConfig as T
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Valid as Valid
import qualified Wasp.Version as WV

spec_AppSpecValid :: Spec
spec_AppSpecValid = do
  describe "validateAppSpec" $ do
    describe "should validate that AppSpec has exactly 1 'app' declaration." $ do
      it "returns no error if there is exactly 1 'app' declaration." $ do
        ASV.validateAppSpec (basicAppSpec {AS.decls = [basicAppDecl, basicRouteDecl]}) `shouldBe` []
      it "returns an error if there is no 'app' declaration." $ do
        ASV.validateAppSpec (basicAppSpec {AS.decls = []})
          `shouldBe` [ Valid.GenericValidationError
                         "You are missing an 'app' declaration in your Wasp app."
                     ]
      it "returns an error if there are 2 'app' declarations." $ do
        ASV.validateAppSpec
          ( basicAppSpec
              { AS.decls =
                  [ AS.Decl.makeDecl "app1" basicApp,
                    AS.Decl.makeDecl "app2" basicApp
                  ]
              }
          )
          `shouldBe` [ Valid.GenericValidationError
                         "You have more than one 'app' declaration in your Wasp app. You have 2."
                     ]

    describe "'waspVersion' validation" $ do
      describe "should validate 'waspVersion' format" $ do
        let basicAppWithVersionRange versionRange =
              basicApp {AS.App.wasp = AS.Wasp.Wasp {AS.Wasp.version = versionRange}}

        let basicAppSpecWithVersionRange versionRange =
              basicAppSpec
                { AS.decls =
                    [ AS.Decl.makeDecl "TestApp" $ basicAppWithVersionRange versionRange,
                      basicRouteDecl
                    ]
                }

        it "returns no error if waspVersion is compatible" $ do
          ASV.validateAppSpec (basicAppSpec {AS.decls = [basicAppDecl, basicRouteDecl]}) `shouldBe` []

        it "returns an error if 'waspVersion' has an incorrect format" $ do
          ASV.validateAppSpec (basicAppSpecWithVersionRange "0.5;2")
            `shouldBe` [ Valid.GenericValidationError
                           "Wasp version should be in the format ^major.minor.patch"
                       ]

        it "returns an error if 'waspVersion' is not compatible" $ do
          let incompatibleWaspVersion = WV.waspVersion {SV.major = SV.major WV.waspVersion + 1}

          ASV.validateAppSpec (basicAppSpecWithVersionRange $ "^" ++ show incompatibleWaspVersion)
            `shouldBe` [ Valid.GenericValidationError $
                           unlines
                             [ "Your Wasp version does not match the app's requirements.",
                               "You are running Wasp " ++ show WV.waspVersion ++ ".",
                               "This app requires Wasp ^" ++ show incompatibleWaspVersion ++ ".",
                               "To install specific version of Wasp, do:",
                               "  curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v x.y.z",
                               "where x.y.z is your desired version.",
                               "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions."
                             ]
                       ]

    describe "auth-related validation" $ do
      let userEntityName = "User"
      let validUserEntity =
            AS.Entity.makeEntity
              ( Psl.Model.Body $
                  Psl.WithCtx.empty
                    <$> [ Psl.Model.ElementField $ makeIdField "id" Psl.Model.String
                        ]
              )
      let validAppAuth =
            AS.Auth.Auth
              { AS.Auth.userEntity = AS.Core.Ref.Ref userEntityName,
                AS.Auth.externalAuthEntity = Nothing,
                AS.Auth.methods =
                  AS.Auth.AuthMethods
                    { AS.Auth.usernameAndPassword = Just AS.Auth.UsernameAndPasswordConfig {AS.Auth.userSignupFields = Nothing},
                      AS.Auth.discord = Nothing,
                      AS.Auth.slack = Nothing,
                      AS.Auth.google = Nothing,
                      AS.Auth.gitHub = Nothing,
                      AS.Auth.keycloak = Nothing,
                      AS.Auth.email = Nothing
                    },
                AS.Auth.onAuthFailedRedirectTo = "/",
                AS.Auth.onAuthSucceededRedirectTo = Nothing,
                AS.Auth.onBeforeSignup = Nothing,
                AS.Auth.onAfterSignup = Nothing,
                AS.Auth.onAfterEmailVerified = Nothing,
                AS.Auth.onBeforeOAuthRedirect = Nothing,
                AS.Auth.onBeforeLogin = Nothing,
                AS.Auth.onAfterLogin = Nothing
              }

      describe "should validate that when a page has authRequired, app.auth is also set." $ do
        let makeSpec appAuth pageAuthRequired =
              basicAppSpec
                { AS.decls =
                    [ AS.Decl.makeDecl "TestApp" $
                        basicApp {AS.App.auth = appAuth},
                      AS.Decl.makeDecl "TestPage" $
                        basicPage {AS.Page.authRequired = pageAuthRequired},
                      AS.Decl.makeDecl userEntityName validUserEntity,
                      basicRouteDecl
                    ]
                }

        it "returns no error if there is no page with authRequired and app.auth is not set" $ do
          ASV.validateAppSpec (makeSpec Nothing Nothing) `shouldBe` []
          ASV.validateAppSpec (makeSpec Nothing (Just False)) `shouldBe` []
        it "returns no error if there is a page with authRequired and app.auth is set" $ do
          ASV.validateAppSpec (makeSpec (Just validAppAuth) (Just True)) `shouldBe` []
        it "returns an error if there is a page with authRequired and app.auth is not set" $ do
          ASV.validateAppSpec (makeSpec Nothing (Just True))
            `shouldBe` [ Valid.GenericValidationError
                           "Expected app.auth to be defined since there are Pages with authRequired set to true."
                       ]
        it "contains expected fields" $ do
          ASV.doesUserEntityContainField (makeSpec Nothing Nothing) "id" `shouldBe` Nothing
          ASV.doesUserEntityContainField (makeSpec (Just validAppAuth) Nothing) "id" `shouldBe` Just True
          ASV.doesUserEntityContainField (makeSpec (Just validAppAuth) Nothing) "missing" `shouldBe` Just False

      describe "should validate that UsernameAndPassword and Email auth cannot used at the same time" $ do
        let makeSpec authMethods userEntity =
              basicAppSpec
                { AS.decls =
                    [ AS.Decl.makeDecl "TestApp" $
                        basicApp
                          { AS.App.auth =
                              Just
                                AS.Auth.Auth
                                  { AS.Auth.methods = authMethods,
                                    AS.Auth.userEntity = AS.Core.Ref.Ref userEntityName,
                                    AS.Auth.externalAuthEntity = Nothing,
                                    AS.Auth.onAuthFailedRedirectTo = "/",
                                    AS.Auth.onAuthSucceededRedirectTo = Nothing,
                                    AS.Auth.onBeforeSignup = Nothing,
                                    AS.Auth.onAfterSignup = Nothing,
                                    AS.Auth.onAfterEmailVerified = Nothing,
                                    AS.Auth.onBeforeOAuthRedirect = Nothing,
                                    AS.Auth.onBeforeLogin = Nothing,
                                    AS.Auth.onAfterLogin = Nothing
                                  },
                            AS.App.emailSender =
                              Just
                                AS.EmailSender.EmailSender
                                  { AS.EmailSender.provider = AS.EmailSender.Mailgun,
                                    AS.EmailSender.defaultFrom = Nothing
                                  }
                          },
                      AS.Decl.makeDecl userEntityName userEntity,
                      basicPageDecl,
                      basicRouteDecl
                    ]
                }
        let emailAuthConfig =
              AS.Auth.EmailAuthConfig
                { AS.Auth.userSignupFields = Nothing,
                  AS.Auth.fromField =
                    AS.EmailSender.EmailFromField
                      { AS.EmailSender.email = "dummy@info.com",
                        AS.EmailSender.name = Nothing
                      },
                  AS.Auth.emailVerification =
                    AS.Auth.EmailVerification.EmailVerificationConfig
                      { AS.Auth.EmailVerification.clientRoute = AS.Core.Ref.Ref basicRouteName,
                        AS.Auth.EmailVerification.getEmailContentFn = Nothing
                      },
                  AS.Auth.passwordReset =
                    AS.Auth.PasswordReset.PasswordResetConfig
                      { AS.Auth.PasswordReset.clientRoute = AS.Core.Ref.Ref basicRouteName,
                        AS.Auth.PasswordReset.getEmailContentFn = Nothing
                      }
                }

        it "returns no error if app.auth is not set" $ do
          ASV.validateAppSpec (makeSpec (AS.Auth.AuthMethods {usernameAndPassword = Nothing, slack = Nothing, discord = Nothing, google = Nothing, keycloak = Nothing, gitHub = Nothing, email = Nothing}) validUserEntity) `shouldBe` []

        it "returns no error if app.auth is set and only one of UsernameAndPassword and Email is used" $ do
          ASV.validateAppSpec
            ( makeSpec
                ( AS.Auth.AuthMethods
                    { usernameAndPassword =
                        Just
                          AS.Auth.UsernameAndPasswordConfig
                            { AS.Auth.userSignupFields = Nothing
                            },
                      discord = Nothing,
                      slack = Nothing,
                      google = Nothing,
                      keycloak = Nothing,
                      gitHub = Nothing,
                      email = Nothing
                    }
                )
                validUserEntity
            )
            `shouldBe` []
          ASV.validateAppSpec (makeSpec (AS.Auth.AuthMethods {usernameAndPassword = Nothing, slack = Nothing, discord = Nothing, google = Nothing, keycloak = Nothing, gitHub = Nothing, email = Just emailAuthConfig}) validUserEntity) `shouldBe` []

        it "returns an error if app.auth is set and both UsernameAndPassword and Email are used" $ do
          ASV.validateAppSpec
            ( makeSpec
                ( AS.Auth.AuthMethods
                    { usernameAndPassword =
                        Just
                          AS.Auth.UsernameAndPasswordConfig
                            { AS.Auth.userSignupFields = Nothing
                            },
                      slack = Nothing,
                      discord = Nothing,
                      google = Nothing,
                      keycloak = Nothing,
                      gitHub = Nothing,
                      email = Just emailAuthConfig
                    }
                )
                validUserEntity
            )
            `shouldContain` [Valid.GenericValidationError "Expected app.auth to use either email or username and password authentication, but not both."]

      describe "should validate that when app.auth is using UsernameAndPassword, user entity is of valid shape." $ do
        let makeSpec appAuth userEntity =
              basicAppSpec
                { AS.decls =
                    [ AS.Decl.makeDecl "TestApp" $
                        basicApp {AS.App.auth = appAuth},
                      AS.Decl.makeDecl userEntityName (userEntity :: AS.Entity.Entity),
                      basicRouteDecl
                    ]
                }
        let invalidUserEntityWithoutIdField =
              AS.Entity.makeEntity
                ( Psl.Model.Body
                    []
                )
        let invalidUserEntityWithoutDefaultAttr =
              AS.Entity.makeEntity
                ( Psl.Model.Body $
                    Psl.WithCtx.empty
                      <$> [ Psl.Model.ElementField $
                              Psl.Model.Field
                                { Psl.Model._name = "id",
                                  Psl.Model._type = Psl.Model.String,
                                  Psl.Model._typeModifiers = [],
                                  Psl.Model._attrs =
                                    [ Psl.Attribute.Attribute
                                        { Psl.Attribute._attrName = "id",
                                          Psl.Attribute._attrArgs = []
                                        }
                                    ]
                                }
                          ]
                )

        it "returns no error if app.auth is not set, regardless of shape of user entity" $ do
          ASV.validateAppSpec (makeSpec Nothing invalidUserEntityWithoutIdField) `shouldBe` []
          ASV.validateAppSpec (makeSpec Nothing validUserEntity) `shouldBe` []
        it "returns no error if app.auth is set and user entity is of valid shape" $ do
          ASV.validateAppSpec (makeSpec (Just validAppAuth) validUserEntity) `shouldBe` []
        it "returns an error if app.auth is set and user entity wihtout an ID field" $ do
          ASV.validateAppSpec (makeSpec (Just validAppAuth) invalidUserEntityWithoutIdField)
            `shouldBe` [ Valid.GenericValidationError
                           "Entity 'User' (referenced by app.auth.userEntity) must have an ID field (specified with the '@id' attribute)"
                       ]
        it "returns an error if app.auth is set and user entity with an ID field that is missing the default attr" $ do
          ASV.validateAppSpec (makeSpec (Just validAppAuth) invalidUserEntityWithoutDefaultAttr)
            `shouldBe` [ Valid.GenericValidationError
                           "Entity 'User' (referenced by app.auth.userEntity) must have an ID field (specified with the '@id' attribute) with a default value"
                       ]

      describe "should validate email sender setup." $ do
        let emailAuthConfig =
              AS.Auth.EmailAuthConfig
                { AS.Auth.userSignupFields = Nothing,
                  AS.Auth.fromField =
                    AS.EmailSender.EmailFromField
                      { AS.EmailSender.email = "dummy@info.com",
                        AS.EmailSender.name = Nothing
                      },
                  AS.Auth.emailVerification =
                    AS.Auth.EmailVerification.EmailVerificationConfig
                      { AS.Auth.EmailVerification.clientRoute = AS.Core.Ref.Ref basicRouteName,
                        AS.Auth.EmailVerification.getEmailContentFn = Nothing
                      },
                  AS.Auth.passwordReset =
                    AS.Auth.PasswordReset.PasswordResetConfig
                      { AS.Auth.PasswordReset.clientRoute = AS.Core.Ref.Ref basicRouteName,
                        AS.Auth.PasswordReset.getEmailContentFn = Nothing
                      }
                }

        let makeSpec emailSender isBuild =
              basicAppSpec
                { AS.isBuild = isBuild,
                  AS.decls =
                    [ AS.Decl.makeDecl "TestApp" $
                        basicApp
                          { AS.App.auth =
                              Just
                                AS.Auth.Auth
                                  { AS.Auth.methods =
                                      AS.Auth.AuthMethods {email = Just emailAuthConfig, usernameAndPassword = Nothing, slack = Nothing, discord = Nothing, google = Nothing, keycloak = Nothing, gitHub = Nothing},
                                    AS.Auth.userEntity = AS.Core.Ref.Ref userEntityName,
                                    AS.Auth.externalAuthEntity = Nothing,
                                    AS.Auth.onAuthFailedRedirectTo = "/",
                                    AS.Auth.onAuthSucceededRedirectTo = Nothing,
                                    AS.Auth.onBeforeSignup = Nothing,
                                    AS.Auth.onAfterSignup = Nothing,
                                    AS.Auth.onAfterEmailVerified = Nothing,
                                    AS.Auth.onBeforeOAuthRedirect = Nothing,
                                    AS.Auth.onBeforeLogin = Nothing,
                                    AS.Auth.onAfterLogin = Nothing
                                  },
                            AS.App.emailSender = emailSender
                          },
                      AS.Decl.makeDecl userEntityName $
                        AS.Entity.makeEntity
                          ( Psl.Model.Body $
                              Psl.WithCtx.empty
                                <$> [ Psl.Model.ElementField $ makeIdField "id" Psl.Model.String
                                    ]
                          ),
                      basicPageDecl,
                      basicRouteDecl
                    ]
                }
        let mailgunEmailSender =
              AS.EmailSender.EmailSender
                { AS.EmailSender.provider = AS.EmailSender.Mailgun,
                  AS.EmailSender.defaultFrom = Nothing
                }

        let dummyEmailSender =
              AS.EmailSender.EmailSender
                { AS.EmailSender.provider = AS.EmailSender.Dummy,
                  AS.EmailSender.defaultFrom = Nothing
                }

        it "returns an error if no email sender is set but email auth is used" $ do
          ASV.validateAppSpec (makeSpec Nothing False) `shouldBe` [Valid.GenericValidationError "app.emailSender must be specified when using email auth. You can use the Dummy email sender for development purposes."]
        it "returns no error if email sender is defined while using email auth" $ do
          ASV.validateAppSpec (makeSpec (Just mailgunEmailSender) False) `shouldBe` []
        it "returns no error if the Dummy email sender is used in development" $ do
          ASV.validateAppSpec (makeSpec (Just dummyEmailSender) False) `shouldBe` []
        it "returns an error if the Dummy email sender is used when building the app" $ do
          ASV.validateAppSpec (makeSpec (Just dummyEmailSender) True)
            `shouldBe` [Valid.GenericValidationError "app.emailSender must not be set to Dummy when building for production."]

    describe "duplicate declarations validation" $ do
      -- Page
      let pageDecl = makeBasicPageDecl "testPage"

      -- Route
      let routeDecl = makeBasicRouteDecl "testRoute" "testPage"

      -- Action
      let actionDecl = makeBasicActionDecl "testAction"

      -- Query
      let queryDecl = makeBasicQueryDecl "testQuery"

      -- Api
      let apiDecl1 = makeBasicApiDecl "testApi" (AS.Api.GET, "/foo/bar")
      -- Using a different route not to trigger duplicate route errors
      let apiDecl2 = makeBasicApiDecl "testApi" (AS.Api.GET, "/different/route")

      -- ApiNamespace
      let apiNamespaceDecl1 = makeBasicApiNamespaceDecl "testApiNamespace" "/foo"
      -- Using a different path not to trigger duplicate route errors
      let apiNamespaceDecl2 = makeBasicApiNamespaceDecl "testApiNamespace" "/different/path"

      -- Crud
      let crudDecl = makeBasicCrudDecl "testCrud" "TestEntity"

      -- Entity
      let entityDecl = makeBasicEntityDecl "TestEntity"

      -- Job
      let jobDecl = makeBasicJobDecl "testJob"

      let testDuplicateDecls decls declTypeName expectedErrorMessage = it ("returns an error if there are duplicate " ++ declTypeName ++ " declarations") $ do
            ASV.validateAppSpec
              ( basicAppSpec
                  { AS.decls = decls ++ [basicRouteDecl],
                    AS.prismaSchema = getPrismaSchemaWithConfig ""
                  }
              )
              `shouldBe` [Valid.GenericValidationError expectedErrorMessage]

      testDuplicateDecls [basicAppDecl, pageDecl, pageDecl] "page" "There are duplicate page declarations with name 'testPage'."
      testDuplicateDecls [basicAppDecl, routeDecl, routeDecl] "route" "There are duplicate route declarations with name 'testRoute'."
      testDuplicateDecls [basicAppDecl, actionDecl, actionDecl] "action" "There are duplicate action declarations with name 'testAction'."
      testDuplicateDecls [basicAppDecl, queryDecl, queryDecl] "query" "There are duplicate query declarations with name 'testQuery'."
      testDuplicateDecls [basicAppDecl, apiDecl1, apiDecl2] "api" "There are duplicate api declarations with name 'testApi'."
      testDuplicateDecls [basicAppDecl, apiNamespaceDecl1, apiNamespaceDecl2] "apiNamespace" "There are duplicate apiNamespace declarations with name 'testApiNamespace'."
      testDuplicateDecls [basicAppDecl, crudDecl, crudDecl, entityDecl] "crud" "There are duplicate crud declarations with name 'testCrud'."
      testDuplicateDecls [basicAppDecl, entityDecl, entityDecl] "entity" "There are duplicate entity declarations with name 'TestEntity'."
      testDuplicateDecls [basicAppDecl, jobDecl, jobDecl] "job" "There are duplicate job declarations with name 'testJob'."

    describe "should validate that there's at least one 'route' declaration" $ do
      it "returns no error if there is at least one 'route' declaration" $ do
        ASV.validateAppSpec (basicAppSpec {AS.decls = [basicAppDecl, basicRouteDecl]}) `shouldBe` []
      it "returns an error if there are no 'route' declarations" $ do
        ASV.validateAppSpec (basicAppSpec {AS.decls = [basicAppDecl]})
          `shouldBe` [ Valid.GenericValidationError
                         "You must have at least one route in your app. You can add it using the 'route' declaration."
                     ]
  where
    makeIdField name typ =
      Psl.Model.Field
        { Psl.Model._name = name,
          Psl.Model._type = typ,
          Psl.Model._typeModifiers =
            [],
          Psl.Model._attrs =
            [ Psl.Attribute.Attribute
                { Psl.Attribute._attrName = "id",
                  Psl.Attribute._attrArgs = []
                },
              Psl.Attribute.Attribute
                { Psl.Attribute._attrName = "default",
                  Psl.Attribute._attrArgs =
                    [ Psl.Argument.ArgUnnamed (Psl.Argument.FuncExpr "autoincrement" [])
                    ]
                }
            ]
        }

    basicApp =
      AS.App.App
        { AS.App.wasp =
            AS.Wasp.Wasp
              { AS.Wasp.version = "^" ++ show WV.waspVersion
              },
          AS.App.title = "Test App",
          AS.App.db =
            Just $
              AS.Db.Db
                { AS.Db.seeds = Nothing,
                  AS.Db.prismaSetupFn = Nothing
                },
          AS.App.server = Nothing,
          AS.App.client = Nothing,
          AS.App.auth = Nothing,
          AS.App.head = Nothing,
          AS.App.emailSender = Nothing,
          AS.App.webSocket = Nothing
        }

    basicAppDecl = AS.Decl.makeDecl "TestApp" basicApp

    basicAppSpec =
      AS.AppSpec
        { AS.decls = [basicAppDecl],
          AS.prismaSchema = getPrismaSchemaWithConfig "",
          AS.waspProjectDir = systemSPRoot SP.</> [SP.reldir|test/|],
          AS.externalCodeFiles = [],
          AS.externalPublicFiles = [],
          AS.packageJson =
            Npm.PackageJson.PackageJson
              { Npm.PackageJson.name = "testApp",
                Npm.PackageJson.dependencies = M.empty,
                Npm.PackageJson.devDependencies = M.empty
              },
          AS.isBuild = False,
          AS.migrationsDir = Nothing,
          AS.devEnvVarsClient = [],
          AS.devEnvVarsServer = [],
          AS.userDockerfileContents = Nothing,
          AS.tailwindConfigFilesRelocators = [],
          AS.devDatabaseUrl = Nothing,
          AS.customViteConfigPath = Nothing,
          AS.srcTsConfigPath = [relfile|tsconfig.json|],
          AS.srcTsConfig =
            T.TsConfig
              { T.compilerOptions =
                  T.CompilerOptions
                    { T._module = Just "esnext",
                      T.composite = Just True,
                      T.target = Just "esnext",
                      T.moduleResolution = Just "bundler",
                      T.jsx = Just "preserve",
                      T.strict = Just True,
                      T.esModuleInterop = Just True,
                      T.isolatedModules = Just True,
                      T.moduleDetection = Just "force",
                      T.lib = Just ["dom", "dom.iterable", "esnext"],
                      T.skipLibCheck = Just True,
                      T.allowJs = Just True,
                      T.outDir = Just ".wasp/out/user"
                    },
                T.include = Just ["src"]
              }
        }

    getPrismaSchemaWithConfig restOfPrismaSource =
      Util.getPrismaSchema
        [trimming|
          datasource db {
            provider = "postgresql"
            url      = env("DATABASE_URL")
          }
          generator client {
            provider = "prisma-client-js"
          }
          ${restOfPrismaSource}
        |]

    basicPage =
      AS.Page.Page
        { AS.Page.component =
            AS.ExtImport.ExtImport
              (AS.ExtImport.ExtImportModule "Home")
              (fromJust $ SP.parseRelFileP "pages/Main"),
          AS.Page.authRequired = Nothing
        }

    basicPageName = "TestPage"

    basicPageDecl = makeBasicPageDecl basicPageName

    basicRouteName = "TestRoute"

    basicRouteDecl = makeBasicRouteDecl basicRouteName basicPageName

    makeBasicPageDecl name =
      AS.Decl.makeDecl
        name
        AS.Page.Page
          { AS.Page.component = dummyExtImport,
            AS.Page.authRequired = Nothing
          }

    makeBasicRouteDecl name pageName =
      AS.Decl.makeDecl
        name
        AS.Route.Route {AS.Route.to = AS.Core.Ref.Ref pageName, AS.Route.path = "/test"}

    makeBasicActionDecl name =
      AS.Decl.makeDecl
        name
        AS.Action.Action
          { AS.Action.auth = Nothing,
            AS.Action.entities = Nothing,
            AS.Action.fn = dummyExtImport
          }

    makeBasicQueryDecl name =
      AS.Decl.makeDecl
        name
        AS.Query.Query
          { AS.Query.auth = Nothing,
            AS.Query.entities = Nothing,
            AS.Query.fn = dummyExtImport
          }

    makeBasicApiDecl name route =
      AS.Decl.makeDecl
        name
        AS.Api.Api
          { AS.Api.fn = dummyExtImport,
            AS.Api.middlewareConfigFn = Nothing,
            AS.Api.entities = Nothing,
            AS.Api.httpRoute = route,
            AS.Api.auth = Nothing
          }

    makeBasicApiNamespaceDecl name path =
      AS.Decl.makeDecl
        name
        AS.ApiNamespace.ApiNamespace
          { AS.ApiNamespace.middlewareConfigFn = dummyExtImport,
            AS.ApiNamespace.path = path
          }

    makeBasicCrudDecl name entityName =
      AS.Decl.makeDecl
        name
        AS.Crud.Crud
          { -- CRUD references testEntity, which is defined below,
            -- it needs to be included in the test declarations.
            AS.Crud.entity = AS.Core.Ref.Ref entityName,
            AS.Crud.operations =
              AS.Crud.CrudOperations
                { AS.Crud.get =
                    Just $
                      AS.Crud.CrudOperationOptions
                        { AS.Crud.isPublic = Nothing,
                          AS.Crud.overrideFn = Nothing
                        },
                  AS.Crud.getAll = Nothing,
                  AS.Crud.create = Nothing,
                  AS.Crud.update = Nothing,
                  AS.Crud.delete = Nothing
                }
          }

    makeBasicEntityDecl name =
      AS.Decl.makeDecl
        name
        (AS.Entity.makeEntity $ Psl.Model.Body $ Psl.WithCtx.empty <$> [Psl.Model.ElementField $ makeIdField "id" Psl.Model.String])

    makeBasicJobDecl name =
      AS.Decl.makeDecl
        name
        AS.Job.Job
          { AS.Job.executor = AS.Job.PgBoss,
            AS.Job.perform =
              AS.Job.Perform
                { AS.Job.fn = dummyExtImport,
                  AS.Job.executorOptions = Nothing
                },
            AS.Job.schedule = Nothing,
            AS.Job.entities = Nothing
          }

    dummyExtImport =
      AS.ExtImport.ExtImport
        (AS.ExtImport.ExtImportModule "Dummy")
        (fromJust $ SP.parseRelFileP "dummy/File")
