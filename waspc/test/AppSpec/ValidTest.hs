{-# LANGUAGE TypeApplications #-}

module AppSpec.ValidTest where

import Data.Maybe (fromJust)
import Fixtures (systemSPRoot)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import qualified Wasp.AppSpec.Entity as AS.Entity
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Valid as ASV
import qualified Wasp.Psl.Ast.Model as PslM
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Version as WV

spec_AppSpecValid :: Spec
spec_AppSpecValid = do
  describe "validateAppSpec" $ do
    describe "should validate that AppSpec has exactly 1 'app' declaration." $ do
      it "returns no error if there is exactly 1 'app' declaration." $ do
        ASV.validateAppSpec (basicAppSpec {AS.decls = [basicAppDecl]}) `shouldBe` []
      it "returns an error if there is no 'app' declaration." $ do
        ASV.validateAppSpec (basicAppSpec {AS.decls = []})
          `shouldBe` [ ASV.GenericValidationError
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
          `shouldBe` [ ASV.GenericValidationError
                         "You have more than one 'app' declaration in your Wasp app. You have 2."
                     ]

    describe "'waspVersion' validation" $ do
      describe "should validate 'waspVersion' format" $ do
        let basicAppWithVersionRange versionRange =
              basicApp {AS.App.wasp = AS.Wasp.Wasp {AS.Wasp.version = versionRange}}

        let basicAppSpecWithVersionRange versionRange =
              basicAppSpec
                { AS.decls =
                    [ AS.Decl.makeDecl "TestApp" $ basicAppWithVersionRange versionRange
                    ]
                }

        it "returns no error if waspVersion is compatible" $ do
          ASV.validateAppSpec basicAppSpec `shouldBe` []

        it "returns an error if 'waspVersion' has an incorrect format" $ do
          ASV.validateAppSpec (basicAppSpecWithVersionRange "0.5;2")
            `shouldBe` [ ASV.GenericValidationError
                           "Wasp version should be in the format ^major.minor.patch"
                       ]

        it "returns an error if 'waspVersion' is not compatible" $ do
          let incompatibleWaspVersion = WV.waspVersion {SV.major = SV.major WV.waspVersion + 1}

          ASV.validateAppSpec (basicAppSpecWithVersionRange $ "^" ++ show incompatibleWaspVersion)
            `shouldBe` [ ASV.GenericValidationError $
                           unlines
                             [ "Your Wasp version does not match the app's requirements.",
                               "You are running Wasp " ++ show WV.waspVersion ++ ".",
                               "This app requires Wasp ^" ++ show incompatibleWaspVersion ++ ".",
                               "To install specific version of Wasp, do:",
                               "  curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s -- -v x.y.z",
                               "where x.y.z is your desired version.",
                               "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions."
                             ]
                       ]

    describe "auth-related validation" $ do
      let userEntityName = "User"
      let validUserEntity =
            AS.Entity.makeEntity
              ( PslM.Body
                  [ PslM.ElementField $ makeBasicPslField "username" PslM.String,
                    PslM.ElementField $ makeBasicPslField "password" PslM.String
                  ]
              )
      let validAppAuth =
            AS.Auth.Auth
              { AS.Auth.userEntity = AS.Core.Ref.Ref userEntityName,
                AS.Auth.externalAuthEntity = Nothing,
                AS.Auth.methods =
                  AS.Auth.AuthMethods
                    { AS.Auth.usernameAndPassword = Just AS.Auth.usernameAndPasswordConfig,
                      AS.Auth.google = Nothing,
                      As.Auth.github = Nothing
                    },
                AS.Auth.onAuthFailedRedirectTo = "/",
                AS.Auth.onAuthSucceededRedirectTo = Nothing
              }

      describe "should validate that when a page has authRequired, app.auth is also set." $ do
        let makeSpec appAuth pageAuthRequired =
              basicAppSpec
                { AS.decls =
                    [ AS.Decl.makeDecl "TestApp" $
                        basicApp {AS.App.auth = appAuth},
                      AS.Decl.makeDecl "TestPage" $
                        basicPage {AS.Page.authRequired = pageAuthRequired},
                      AS.Decl.makeDecl userEntityName validUserEntity
                    ]
                }

        it "returns no error if there is no page with authRequired and app.auth is not set" $ do
          ASV.validateAppSpec (makeSpec Nothing Nothing) `shouldBe` []
          ASV.validateAppSpec (makeSpec Nothing (Just False)) `shouldBe` []
        it "returns no error if there is a page with authRequired and app.auth is set" $ do
          ASV.validateAppSpec (makeSpec (Just validAppAuth) (Just True)) `shouldBe` []
        it "returns an error if there is a page with authRequired and app.auth is not set" $ do
          ASV.validateAppSpec (makeSpec Nothing (Just True))
            `shouldBe` [ ASV.GenericValidationError
                           "Expected app.auth to be defined since there are Pages with authRequired set to true."
                       ]

      describe "should validate that when app.auth is using UsernameAndPassword, user entity is of valid shape." $ do
        let makeSpec appAuth userEntity =
              basicAppSpec
                { AS.decls =
                    [ AS.Decl.makeDecl "TestApp" $
                        basicApp {AS.App.auth = appAuth},
                      AS.Decl.makeDecl userEntityName (userEntity :: AS.Entity.Entity)
                    ]
                }
        let invalidUserEntity =
              AS.Entity.makeEntity
                ( PslM.Body
                    [ PslM.ElementField $ makeBasicPslField "email" PslM.String,
                      PslM.ElementField $ makeBasicPslField "password" PslM.String
                    ]
                )
        let invalidUserEntity2 =
              AS.Entity.makeEntity
                ( PslM.Body
                    [ PslM.ElementField $ makeBasicPslField "username" PslM.String
                    ]
                )

        it "returns no error if app.auth is not set, regardless of shape of user entity" $ do
          ASV.validateAppSpec (makeSpec Nothing invalidUserEntity) `shouldBe` []
          ASV.validateAppSpec (makeSpec Nothing validUserEntity) `shouldBe` []
        it "returns no error if app.auth is set and user entity is of valid shape" $ do
          ASV.validateAppSpec (makeSpec (Just validAppAuth) validUserEntity) `shouldBe` []
        it "returns an error if app.auth is set and user entity is of invalid shape" $ do
          ASV.validateAppSpec (makeSpec (Just validAppAuth) invalidUserEntity)
            `shouldBe` [ ASV.GenericValidationError
                           "Expected an Entity referenced by app.auth.userEntity to have field 'username' of type 'String'."
                       ]
          ASV.validateAppSpec (makeSpec (Just validAppAuth) invalidUserEntity2)
            `shouldBe` [ ASV.GenericValidationError
                           "Expected an Entity referenced by app.auth.userEntity to have field 'password' of type 'String'."
                       ]
  where
    makeBasicPslField name typ =
      PslM.Field
        { PslM._name = name,
          PslM._type = typ,
          PslM._typeModifiers = [],
          PslM._attrs = []
        }

    basicApp =
      AS.App.App
        { AS.App.wasp =
            AS.Wasp.Wasp
              { AS.Wasp.version = "^" ++ show WV.waspVersion
              },
          AS.App.title = "Test App",
          AS.App.db = Nothing,
          AS.App.server = Nothing,
          AS.App.client = Nothing,
          AS.App.auth = Nothing,
          AS.App.dependencies = Nothing,
          AS.App.head = Nothing
        }

    basicAppDecl = AS.Decl.makeDecl "TestApp" basicApp

    basicAppSpec =
      AS.AppSpec
        { AS.decls = [basicAppDecl],
          AS.waspProjectDir = systemSPRoot SP.</> [SP.reldir|test/|],
          AS.externalClientFiles = [],
          AS.externalServerFiles = [],
          AS.externalSharedFiles = [],
          AS.isBuild = False,
          AS.migrationsDir = Nothing,
          AS.dotEnvServerFile = Nothing,
          AS.dotEnvClientFile = Nothing,
          AS.userDockerfileContents = Nothing,
          AS.configFiles = []
        }

    basicPage =
      AS.Page.Page
        { AS.Page.component =
            AS.ExtImport.ExtImport
              (AS.ExtImport.ExtImportModule "Home")
              (fromJust $ SP.parseRelFileP "pages/Main"),
          AS.Page.authRequired = Nothing
        }
