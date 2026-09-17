module ServerRoutesTest where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Fixtures (systemSPRoot)
import StrongPath (relfile)
import qualified StrongPath as SP
import Test.Hspec
import qualified Util.Prisma as Util
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Auth.EmailVerification as AS.Auth.EmailVerification
import qualified Wasp.AppSpec.App.Auth.PasswordReset as AS.Auth.PasswordReset
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.App.WebSocket as AS.App.WS
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Query as AS.Query
import qualified Wasp.ExternalConfig.Npm.PackageJson as Npm.PackageJson
import qualified Wasp.Project.BuildType as BuildType
import Wasp.ServerRoutes
  ( ServerRoute (..),
    ServerRouteHttpMethods (..),
    ServerRouteOwner (..),
    ServerRoutePath (..),
  )
import qualified Wasp.ServerRoutes as ServerRoutes

spec_ServerRoutes :: Spec
spec_ServerRoutes = do
  describe "getWaspServerRoutes" $ do
    it "lists only the liveness route for an app that uses nothing else" $ do
      ServerRoutes.getWaspServerRoutes (makeSpec basicApp [])
        `shouldBe` [ServerRoute LivenessRoute getAndHead (ExactPath "/up")]

    it "lists every route of an app that uses everything, in the order the server registers them" $ do
      let spec =
            makeSpec
              basicApp {AS.App.auth = Just authWithEveryKindOfMethod, AS.App.webSocket = Just webSocket}
              [ AS.Decl.makeDecl "getTasks" query,
                AS.Decl.makeDecl "tasks" crudWithGetAndDelete
              ]
      map showRoute (ServerRoutes.getWaspServerRoutes spec)
        `shouldBe` [ "ANY /socket.io/*",
                     "GET,HEAD /auth/me",
                     "POST /auth/logout",
                     "POST /auth/exchange-code",
                     "GET,HEAD /auth/google/login",
                     "GET,HEAD /auth/google/callback",
                     "GET,HEAD /auth/github/login",
                     "GET,HEAD /auth/github/callback",
                     "POST /auth/email/login",
                     "POST /auth/email/signup",
                     "POST /auth/email/request-password-reset",
                     "POST /auth/email/reset-password",
                     "POST /auth/email/verify-email",
                     "POST /operations/get-tasks",
                     "POST /crud/tasks/get",
                     "POST /crud/tasks/delete",
                     "GET,HEAD /up"
                   ]

    it "lists the username routes when username and password auth is used" $ do
      let spec = makeSpec basicApp {AS.App.auth = Just authWithUsernameAndPassword} []
      map showRoute (ServerRoutes.getAuthRoutes spec)
        `shouldBe` [ "GET,HEAD /auth/me",
                     "POST /auth/logout",
                     "POST /auth/username/login",
                     "POST /auth/username/signup"
                   ]

  describe "getUserApiRoutes" $ do
    let getApiRoutes httpRoute = ServerRoutes.getUserApiRoutes $ makeSpec basicApp [AS.Decl.makeDecl "myApi" $ makeApi httpRoute]

    it "spells out an api path that has no pattern in it" $ do
      getApiRoutes (AS.Api.POST, "/webhooks/stripe")
        `shouldBe` [ServerRoute (UserApiRoute "myApi") (OnlyHttpMethods [AS.Api.POST]) (ExactPath "/webhooks/stripe")]
    it "claims the subtree under the static beginning of an api path that is a pattern" $ do
      getApiRoutes (AS.Api.GET, "/files/:id/raw")
        `shouldBe` [ServerRoute (UserApiRoute "myApi") getAndHead (SubtreePath "/files")]
    it "claims the whole server for an api path that starts with a pattern" $ do
      getApiRoutes (AS.Api.ALL, "/*splat")
        `shouldBe` [ServerRoute (UserApiRoute "myApi") AnyHttpMethod (SubtreePath "/")]

  describe "getServerRoutes" $ do
    it "lists Wasp's routes ahead of the user's apis" $ do
      let spec = makeSpec basicApp [AS.Decl.makeDecl "myApi" $ makeApi (AS.Api.POST, "/webhook")]
      map showRoute (ServerRoutes.getServerRoutes spec) `shouldBe` ["GET,HEAD /up", "POST /webhook"]

  describe "doRoutesOverlap" $ do
    let exactRoute httpMethods = ServerRoute (UserApiRoute "a") httpMethods . ExactPath
    let subtreeRoute httpMethods = ServerRoute (UserApiRoute "b") httpMethods . SubtreePath
    let onlyPost = OnlyHttpMethods [AS.Api.POST]

    it "is true for the same path and method" $
      ServerRoutes.doRoutesOverlap (exactRoute onlyPost "/foo") (exactRoute onlyPost "/foo") `shouldBe` True
    it "ignores casing and trailing slashes, as Express does" $
      ServerRoutes.doRoutesOverlap (exactRoute onlyPost "/foo") (exactRoute onlyPost "/FOO/") `shouldBe` True
    it "is false when the routes share no method" $
      ServerRoutes.doRoutesOverlap (exactRoute onlyPost "/foo") (exactRoute getAndHead "/foo") `shouldBe` False
    it "is true when one of the routes answers on any method" $
      ServerRoutes.doRoutesOverlap (exactRoute AnyHttpMethod "/foo") (exactRoute getAndHead "/foo") `shouldBe` True
    it "is true for a path under a subtree" $
      ServerRoutes.doRoutesOverlap (exactRoute onlyPost "/foo/bar") (subtreeRoute onlyPost "/foo") `shouldBe` True
    it "is false for a path that only starts with the same letters as a subtree" $
      ServerRoutes.doRoutesOverlap (exactRoute onlyPost "/foobar") (subtreeRoute onlyPost "/foo") `shouldBe` False
    it "is true for a subtree inside another subtree" $
      ServerRoutes.doRoutesOverlap (subtreeRoute onlyPost "/foo/bar") (subtreeRoute onlyPost "/foo") `shouldBe` True

  describe "mayServerHaveRoutesUnknownToWasp" $ do
    it "is false without a server setupFn" $
      ServerRoutes.mayServerHaveRoutesUnknownToWasp (makeSpec basicApp []) `shouldBe` False
    it "is true with a server setupFn" $ do
      let server = AS.App.Server.Server {AS.App.Server.setupFn = Just dummyExtImport, AS.App.Server.middlewareConfigFn = Nothing, AS.App.Server.envValidationSchema = Nothing}
      ServerRoutes.mayServerHaveRoutesUnknownToWasp (makeSpec basicApp {AS.App.server = Just server} []) `shouldBe` True
  where
    getAndHead = OnlyHttpMethods [AS.Api.GET, AS.Api.HEAD]

    showRoute route = showHttpMethods (httpMethods route) ++ " " ++ showPath (path route)

    showHttpMethods AnyHttpMethod = "ANY"
    showHttpMethods (OnlyHttpMethods methods) = drop 1 $ concatMap ((',' :) . show) methods

    showPath (ExactPath exactPath) = exactPath
    showPath (SubtreePath subtreePath) = subtreePath ++ "/*"

    makeSpec app decls =
      AS.AppSpec
        { AS.decls = AS.Decl.makeDecl "TestApp" app : decls,
          AS.prismaSchema = Util.getPrismaSchema "",
          AS.waspProjectDir = systemSPRoot SP.</> [SP.reldir|test/|],
          AS.externalCodeFiles = [],
          AS.packageJson =
            Npm.PackageJson.PackageJson
              { Npm.PackageJson.name = "testApp",
                Npm.PackageJson.version = Nothing,
                Npm.PackageJson.dependencies = M.empty,
                Npm.PackageJson.devDependencies = M.empty,
                Npm.PackageJson.workspaces = Nothing,
                Npm.PackageJson.wasp = Nothing
              },
          AS.buildType = BuildType.Development,
          AS.migrationsDir = Nothing,
          AS.devEnvVarsClient = [],
          AS.devEnvVarsServer = [],
          AS.userDockerfileContents = Nothing,
          AS.devDatabaseUrl = Nothing,
          AS.srcTsConfigPath = [relfile|tsconfig.json|]
        }

    basicApp =
      AS.App.App
        { AS.App.wasp = AS.Wasp.Wasp {AS.Wasp.version = "^0.0.0"},
          AS.App.title = "Test App",
          AS.App.deployment = Nothing,
          AS.App.db = Nothing,
          AS.App.server = Nothing,
          AS.App.client = Nothing,
          AS.App.auth = Nothing,
          AS.App.head = Nothing,
          AS.App.emailSender = Nothing,
          AS.App.webSocket = Nothing
        }

    webSocket = AS.App.WS.WebSocket {AS.App.WS.fn = dummyExtImport, AS.App.WS.autoConnect = Nothing}

    authWithUsernameAndPassword =
      makeAuth
        noAuthMethods
          { AS.Auth.usernameAndPassword = Just AS.Auth.UsernameAndPasswordConfig {AS.Auth.userSignupFields = Nothing}
          }

    authWithEveryKindOfMethod =
      makeAuth
        noAuthMethods
          { AS.Auth.google = Just externalAuthConfig,
            AS.Auth.gitHub = Just externalAuthConfig,
            AS.Auth.email = Just emailAuthConfig
          }

    makeAuth authMethods =
      AS.Auth.Auth
        { AS.Auth.userEntity = AS.Core.Ref.Ref "User",
          AS.Auth.methods = authMethods,
          AS.Auth.onAuthFailedRedirectTo = "/",
          AS.Auth.onAuthSucceededRedirectTo = Nothing,
          AS.Auth.onBeforeSignup = Nothing,
          AS.Auth.onAfterSignup = Nothing,
          AS.Auth.onAfterEmailVerified = Nothing,
          AS.Auth.onBeforeOAuthRedirect = Nothing,
          AS.Auth.onBeforeLogin = Nothing,
          AS.Auth.onAfterLogin = Nothing
        }

    noAuthMethods =
      AS.Auth.AuthMethods
        { AS.Auth.usernameAndPassword = Nothing,
          AS.Auth.discord = Nothing,
          AS.Auth.slack = Nothing,
          AS.Auth.google = Nothing,
          AS.Auth.gitHub = Nothing,
          AS.Auth.keycloak = Nothing,
          AS.Auth.microsoft = Nothing,
          AS.Auth.email = Nothing
        }

    externalAuthConfig = AS.Auth.ExternalAuthConfig {AS.Auth.configFn = Nothing, AS.Auth.userSignupFields = Nothing}

    emailAuthConfig =
      AS.Auth.EmailAuthConfig
        { AS.Auth.userSignupFields = Nothing,
          AS.Auth.fromField = AS.EmailSender.EmailFromField {AS.EmailSender.email = "dummy@info.com", AS.EmailSender.name = Nothing},
          AS.Auth.emailVerification =
            AS.Auth.EmailVerification.EmailVerificationConfig
              { AS.Auth.EmailVerification.clientRoute = AS.Core.Ref.Ref "TestRoute",
                AS.Auth.EmailVerification.getEmailContentFn = Nothing
              },
          AS.Auth.passwordReset =
            AS.Auth.PasswordReset.PasswordResetConfig
              { AS.Auth.PasswordReset.clientRoute = AS.Core.Ref.Ref "TestRoute",
                AS.Auth.PasswordReset.getEmailContentFn = Nothing
              }
        }

    query = AS.Query.Query {AS.Query.auth = Nothing, AS.Query.entities = Nothing, AS.Query.fn = dummyExtImport}

    crudWithGetAndDelete =
      AS.Crud.Crud
        { AS.Crud.entity = AS.Core.Ref.Ref "Task",
          AS.Crud.operations =
            AS.Crud.CrudOperations
              { AS.Crud.get = Just crudOperationOptions,
                AS.Crud.getAll = Nothing,
                AS.Crud.create = Nothing,
                AS.Crud.update = Nothing,
                AS.Crud.delete = Just crudOperationOptions
              }
        }

    crudOperationOptions = AS.Crud.CrudOperationOptions {AS.Crud.isPublic = Nothing, AS.Crud.overrideFn = Nothing}

    makeApi httpRoute =
      AS.Api.Api
        { AS.Api.fn = dummyExtImport,
          AS.Api.middlewareConfigFn = Nothing,
          AS.Api.entities = Nothing,
          AS.Api.httpRoute = httpRoute,
          AS.Api.auth = Nothing
        }

    dummyExtImport =
      AS.ExtImport.ExtImport
        (AS.ExtImport.ExtImportModule "Dummy")
        (fromJust $ SP.parseRelFileP "dummy/File")
        Nothing
