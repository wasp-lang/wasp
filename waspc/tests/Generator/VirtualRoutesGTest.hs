module Generator.VirtualRoutesGTest where

import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Fixtures (systemSPRoot)
import NeatInterpolation (trimming)
import StrongPath (relfile)
import qualified StrongPath as SP
import Test.Hspec
import qualified Util.Prisma as Util
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import qualified Wasp.ExternalConfig.Npm.PackageJson as Npm.PackageJson
import Wasp.Generator.FileDraft (FileDraft (..))
import Wasp.Generator.FileDraft.TemplateFileDraft (TemplateFileDraft (..))
import Wasp.Generator.Monad (runGenerator)
import qualified Wasp.Generator.NpmWorkspaces as NW
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualWaspModulesPlugin.VirtualRoutesG (genVirtualRoutesTsx)
import qualified Wasp.Generator.Templates as Templates
import qualified Wasp.Project.BuildType as BuildType
import qualified Wasp.Version as WV

spec_GeneratorVirtualRoutesG :: Spec
spec_GeneratorVirtualRoutesG = do
  describe "genVirtualRoutesTsx" $ do
    it "imports the page once when two eager routes use the same page" $ do
      rendered <-
        renderRoutesTsx
          [ routeToMainPage "RootRoute" "/" eager,
            routeToMainPage "SecondRoute" "/second" eager
          ]
      T.count mainPageImport rendered `shouldBe` 1
      T.count "RootRoute: {" rendered `shouldBe` 1
      T.count "SecondRoute: {" rendered `shouldBe` 1
      T.count "Component: MainPage," rendered `shouldBe` 2

    it "imports each page once when eager routes use different pages" $ do
      rendered <-
        renderRoutesTsx
          [ routeToMainPage "RootRoute" "/" eager,
            routeToOtherPage "SecondRoute" "/second" eager
          ]
      T.count mainPageImport rendered `shouldBe` 1
      T.count otherPageImport rendered `shouldBe` 1
      T.count "Component: MainPage," rendered `shouldBe` 1
      T.count "Component: OtherPage," rendered `shouldBe` 1

    it "imports the page once when one route is lazy and the other eager" $ do
      rendered <-
        renderRoutesTsx
          [ routeToMainPage "RootRoute" "/" lazy,
            routeToMainPage "SecondRoute" "/second" eager
          ]
      T.count mainPageImport rendered `shouldBe` 1
      T.count "lazy: async () =>" rendered `shouldBe` 1
      T.count mainPageLazyImport rendered `shouldBe` 1
      T.count "Component: MainPage," rendered `shouldBe` 1

    it "does not import the page at the top level when all routes to it are lazy" $ do
      rendered <-
        renderRoutesTsx
          [ routeToMainPage "RootRoute" "/" lazy,
            routeToMainPage "SecondRoute" "/second" lazy
          ]
      T.count mainPageImport rendered `shouldBe` 0
      T.count "lazy: async () =>" rendered `shouldBe` 2
      T.count mainPageLazyImport rendered `shouldBe` 2
  where
    eager = Just False
    lazy = Just True

    mainPageImport = pageImport "MainPage"
    otherPageImport = pageImport "OtherPage"
    mainPageLazyImport = pageLazyImport "MainPage"

    pageImport pageName =
      T.concat ["import { ", T.pack pageName, " } from ", jsStringLiteral ("./src/" <> pageName)]

    pageLazyImport pageName =
      T.concat ["const Component = await import(", jsStringLiteral ("./src/" <> pageName), ").then(m => m.", T.pack pageName, ");"]

    -- Matches Wasp.Util.Js.makeJsStringLiteral: module paths in the generated
    -- code are double-quoted JS string literals.
    jsStringLiteral path = T.concat ["\"", T.pack path, "\""]

    routeToMainPage name path lazy' = mkRoute name path "MainPage" lazy'
    routeToOtherPage name path lazy' = mkRoute name path "OtherPage" lazy'

    mkRoute name path pageName lazy' =
      ( name,
        AS.Route.Route
          { AS.Route.to = AS.Core.Ref.Ref pageName,
            AS.Route.path = path,
            AS.Route.lazy = lazy',
            AS.Route.prerender = []
          }
      )

    renderRoutesTsx routes =
      case runGenerator (genVirtualRoutesTsx (appSpec routes)) of
        (_warnings, Right (FileDraftTemplateFd draft)) ->
          case _tmplData draft of
            Just tmplData -> Templates.compileAndRenderTemplate (_srcPathInTmplDir draft) tmplData
            Nothing -> error "expected routes.tsx to carry template data"
        (_warnings, Right _) -> error "expected routes.tsx to be a template file draft"
        (_warnings, Left generatorErrors) -> error $ "generator failed: " ++ show generatorErrors

    appSpec routes =
      AS.AppSpec
        { AS.decls = [appDecl, mainPageDecl, otherPageDecl] ++ map (uncurry AS.Decl.makeDecl) routes,
          AS.prismaSchema = prismaSchema,
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

    prismaSchema =
      Util.getPrismaSchema
        [trimming|
          datasource db {
            provider = "postgresql"
            url      = env("DATABASE_URL")
          }
          generator client {
            provider = "prisma-client-js"
          }
        |]

    appDecl =
      AS.Decl.makeDecl "TestApp" $
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

    mainPageDecl = mkPageDecl "MainPage"
    otherPageDecl = mkPageDecl "OtherPage"

    mkPageDecl name =
      AS.Decl.makeDecl
        name
        AS.Page.Page
          { AS.Page.component =
              AS.ExtImport.ExtImport
                (AS.ExtImport.ExtImportField name)
                (fromJust $ SP.parseRelFileP name)
                Nothing,
            AS.Page.authRequired = Nothing
          }
