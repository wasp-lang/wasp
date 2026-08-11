module Wasp.Cli.AppComponentsTest where

import Data.Maybe (fromJust)
import StrongPath (relfile)
import qualified StrongPath as SP
import qualified System.FilePath as FP
import Test.Hspec
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import Wasp.Cli.AppComponents (makeDevRunConfigs)
import qualified Wasp.ExternalConfig.Npm.PackageJson as Npm.PackageJson
import qualified Wasp.Generator.Server.RunConfig as Server
import qualified Wasp.Generator.WebApp.RunConfig as Client
import qualified Wasp.Project.BuildType as BuildType
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Version as WV

spec_makeDevRunConfigs :: Spec
spec_makeDevRunConfigs = do
  describe "makeDevRunConfigs" $ do
    it "wires each component's URL into the other" $ do
      let (client, server) = makeDevRunConfigs basicAppSpec
      Client.serverUrl client `shouldBe` Just "http://localhost:3001"
      Server.clientUrl server `shouldBe` Just "http://localhost:3000/"

    it "includes the client's base dir in its URL" $ do
      let (client, server) = makeDevRunConfigs $ appSpecWithClientBaseDir "/app"
      Client.url client `shouldBe` "http://localhost:3000/app/"
      Server.clientUrl server `shouldBe` Just "http://localhost:3000/app/"

    it "omits the peer URL env vars while the components aren't wired" $ do
      Client.devEnvVars (Client.makeDefault basicAppSpec) `shouldBe` []
      Server.devEnvVars Server.makeDefault
        `shouldBe` [("WASP_SERVER_URL", "http://localhost:3001")]

    it "sets the peer URL env vars once the components are wired" $ do
      let (client, server) = makeDevRunConfigs basicAppSpec
      Client.devEnvVars client
        `shouldBe` [("REACT_APP_API_URL", "http://localhost:3001")]
      Server.devEnvVars server
        `shouldBe` [ ("WASP_WEB_CLIENT_URL", "http://localhost:3000/"),
                     ("WASP_SERVER_URL", "http://localhost:3001")
                   ]
  where
    appSpecWithClientBaseDir clientBaseDir =
      basicAppSpec
        { AS.decls =
            [ AS.Decl.makeDecl "TestApp" $
                basicApp
                  { AS.App.client =
                      Just $
                        AS.App.Client.Client
                          { AS.App.Client.setupFn = Nothing,
                            AS.App.Client.rootComponent = Nothing,
                            AS.App.Client.baseDir = Just clientBaseDir,
                            AS.App.Client.envValidationSchema = Nothing
                          }
                  }
            ]
        }

    basicAppSpec =
      AS.AppSpec
        { AS.decls = [AS.Decl.makeDecl "TestApp" basicApp],
          AS.prismaSchema = Psl.Schema.Schema [],
          AS.waspProjectDir = systemSPRoot SP.</> [SP.reldir|test/|],
          AS.externalCodeFiles = [],
          AS.packageJson =
            Npm.PackageJson.PackageJson
              { Npm.PackageJson.name = "testApp",
                Npm.PackageJson.version = Nothing,
                Npm.PackageJson.dependencies = mempty,
                Npm.PackageJson.devDependencies = mempty,
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
        { AS.App.wasp =
            AS.Wasp.Wasp
              { AS.Wasp.version = "^" ++ show WV.waspVersion
              },
          AS.App.title = "Test App",
          AS.App.head = Nothing,
          AS.App.auth = Nothing,
          AS.App.server = Nothing,
          AS.App.client = Nothing,
          AS.App.db = Nothing,
          AS.App.emailSender = Nothing,
          AS.App.webSocket = Nothing
        }

    systemSPRoot :: SP.Path' SP.Abs (SP.Dir d)
    systemSPRoot = fromJust $ SP.parseAbsDir $ if FP.pathSeparator == '\\' then "C:\\" else "/"
