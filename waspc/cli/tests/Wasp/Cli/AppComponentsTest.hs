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
import qualified Wasp.Generator.Client as Client
import qualified Wasp.Generator.Server as Server
import qualified Wasp.Project.BuildType as BuildType
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Version as WV

spec_makeDevRunConfigs :: Spec
spec_makeDevRunConfigs = do
  describe "makeDevRunConfigs" $ do
    it "wires each component's URL into the other" $ do
      let (client, server) = makeDevRunConfigs basicAppSpec 3123 3456
      Client.serverUrl client `shouldBe` Just "http://localhost:3456"
      Server.clientUrl server `shouldBe` Just "http://localhost:3123/"

    it "includes the client's base dir in its URL" $ do
      let (client, server) = makeDevRunConfigs (appSpecWithClientBaseDir "/app") 3123 3456
      Client.url client `shouldBe` "http://localhost:3123/app/"
      Server.clientUrl server `shouldBe` Just "http://localhost:3123/app/"

    it "omits the peer URL env vars while the components aren't wired" $ do
      Client.devEnvVars (Client.make basicAppSpec 3123)
        `shouldBe` [("PORT", "3123")]
      Server.devEnvVars (Server.make 3456)
        `shouldBe` [ ("PORT", "3456"),
                     ("WASP_SERVER_URL", "http://localhost:3456")
                   ]

    it "sets the peer URL env vars once the components are wired" $ do
      let (client, server) = makeDevRunConfigs basicAppSpec Client.defaultPort Server.defaultPort
      Client.devEnvVars client
        `shouldBe` [ ("PORT", "3000"),
                     ("REACT_APP_API_URL", "http://localhost:3001")
                   ]
      Server.devEnvVars server
        `shouldBe` [ ("PORT", "3001"),
                     ("WASP_SERVER_URL", "http://localhost:3001"),
                     ("WASP_WEB_CLIENT_URL", "http://localhost:3000/")
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
