module Generator.ServerGenerator.CommonTest where

import qualified Data.Map as M
import qualified Data.Set as S
import Fixtures (systemSPRoot)
import NeatInterpolation (trimming)
import StrongPath (relfile)
import qualified StrongPath as SP
import Test.Hspec
import qualified Util.Prisma as Util
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Server as AS.Server
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import qualified Wasp.ExternalConfig.Npm.PackageJson as Npm.PackageJson
import qualified Wasp.Generator.NpmWorkspaces as NW
import Wasp.Generator.ServerGenerator.Common (getServerBasePath, getServerBasePathPrefix, getWebSocketPath)
import qualified Wasp.Project.BuildType as BuildType
import qualified Wasp.Version as WV

spec_ServerGeneratorCommon :: Spec
spec_ServerGeneratorCommon = do
  describe "getServerBasePath" $ do
    it "is the root when app.server.basePath is not set" $
      getServerBasePath (makeSpecWithBasePath Nothing) `shouldBe` "/"
    it "is the root when app.server.basePath is the root" $
      getServerBasePath (makeSpecWithBasePath (Just "/")) `shouldBe` "/"
    it "is app.server.basePath when it is nested" $
      getServerBasePath (makeSpecWithBasePath (Just "/api/v1")) `shouldBe` "/api/v1"

  describe "getServerBasePathPrefix" $ do
    it "is empty when app.server.basePath is not set" $
      getServerBasePathPrefix (makeSpecWithBasePath Nothing) `shouldBe` ""
    it "is empty when app.server.basePath is the root" $
      getServerBasePathPrefix (makeSpecWithBasePath (Just "/")) `shouldBe` ""
    it "is app.server.basePath when it is nested" $
      getServerBasePathPrefix (makeSpecWithBasePath (Just "/api/v1")) `shouldBe` "/api/v1"

  describe "getWebSocketPath" $ do
    it "is the socket.io default when app.server.basePath is the root" $
      getWebSocketPath (makeSpecWithBasePath (Just "/")) `shouldBe` "/socket.io"
    it "is under app.server.basePath when it is nested" $
      getWebSocketPath (makeSpecWithBasePath (Just "/api")) `shouldBe` "/api/socket.io"
  where
    makeSpecWithBasePath basePath =
      basicAppSpec
        { AS.decls =
            [ AS.Decl.makeDecl
                "TestApp"
                basicApp
                  { AS.App.server =
                      Just $
                        AS.Server.Server
                          { AS.Server.setupFn = Nothing,
                            AS.Server.middlewareConfigFn = Nothing,
                            AS.Server.envValidationSchema = Nothing,
                            AS.Server.basePath = basePath
                          }
                  }
            ]
        }

    basicApp =
      AS.App.App
        { AS.App.wasp = AS.Wasp.Wasp {AS.Wasp.version = "^" ++ show WV.waspVersion},
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

    basicAppSpec =
      AS.AppSpec
        { AS.decls = [],
          AS.prismaSchema = basicPrismaSchema,
          AS.waspProjectDir = systemSPRoot SP.</> [SP.reldir|test/|],
          AS.externalCodeFiles = [],
          AS.packageJson =
            Npm.PackageJson.PackageJson
              { Npm.PackageJson.name = "testApp",
                Npm.PackageJson.version = Nothing,
                Npm.PackageJson.dependencies = M.empty,
                Npm.PackageJson.devDependencies = M.empty,
                Npm.PackageJson.workspaces = Just $ S.toList NW.requiredWorkspaceGlobs,
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

    basicPrismaSchema =
      Util.getPrismaSchema
        [trimming|
          datasource db {
            provider = "postgresql"
            url = env("DATABASE_URL")
          }
        |]
