module Generator.InstalledNpmDepsTest where

import Data.Maybe (fromJust)
import qualified Fixtures.AppSpec as Fixtures
import qualified StrongPath as SP
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldReturn)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import Wasp.Generator.InstalledNpmDeps (findNotInstalledNpmDeps)

spec_findNotInstalledNpmDeps :: Spec
spec_findNotInstalledNpmDeps =
  describe "findNotInstalledNpmDeps" $ do
    it "finds the Wasp SDK in the project's node_modules" $
      withWorkspace $ \_ projectDir -> do
        installSdk projectDir
        findNotInstalledNpmDeps (appSpecIn projectDir) `shouldReturn` []
    it "finds the Wasp SDK hoisted to a parent dir's node_modules" $
      withWorkspace $ \workspaceDir projectDir -> do
        installSdk workspaceDir
        findNotInstalledNpmDeps (appSpecIn projectDir) `shouldReturn` []
    it "reports the Wasp SDK when no node_modules has it" $
      withWorkspace $ \_ projectDir ->
        findNotInstalledNpmDeps (appSpecIn projectDir) `shouldReturn` ["wasp"]
  where
    -- An npm workspace with the Wasp project in `apps/web`.
    withWorkspace test =
      withSystemTempDirectory "wasp-installed-npm-deps-test" $ \tempDir' -> do
        workspaceDir <- canonicalizePath tempDir'
        let projectDir = workspaceDir </> "apps" </> "web"
        createDirectoryIfMissing True projectDir
        test workspaceDir projectDir

    installSdk dir = createDirectoryIfMissing True $ dir </> "node_modules" </> "wasp"

    appSpecIn projectDir =
      Fixtures.basicAppSpec
        { AS.waspProjectDir = fromJust $ SP.parseAbsDir projectDir,
          -- No features, so the Wasp SDK is the only package it looks for.
          AS.decls = [AS.Decl.makeDecl appWithoutFeatures]
        }

    appWithoutFeatures =
      AS.App.App
        { AS.App.name = "TestApp",
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
