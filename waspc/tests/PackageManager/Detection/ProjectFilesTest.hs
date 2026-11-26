module PackageManager.Detection.ProjectFilesTest where

import Control.Monad (forM_, (>=>))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.IO as T
import NeatInterpolation (trimming)
import StrongPath (Abs, Dir, Path', reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import qualified System.FilePath as FP
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Wasp.PackageManager.Detection.Common (Detection (..), DetectionSource (..))
import Wasp.PackageManager.Detection.ProjectFiles (detectFromProjectFiles)
import Wasp.PackageManager.MarkerFiles (PackageRootDir)
import Wasp.PackageManager.PackageManager (PackageManager (..))

{- HLINT ignore spec_PackageManager_Detection_ProjectFiles "Avoid npm and npx" -}

spec_PackageManager_Detection_ProjectFiles :: Spec
spec_PackageManager_Detection_ProjectFiles = do
  describe "detectFromProjectFiles" $ do
    it "finds nothing in an empty project" $
      withProject [".git/HEAD" ~> ""] $ \projectRoot ->
        detectFromProjectFiles projectRoot `shouldReturn` foundNothing

    describe "in the project directory" $ do
      it "detects the packageManager field"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package.json" ~> [trimming|{ "packageManager": "pnpm@10.20.0" }|]
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Pnpm (PackageManagerField $ projectRoot </> [relfile|package.json|])

      it "detects the devEngines field"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package.json" ~> [trimming|{ "devEngines": { "packageManager": { "name": "bun" } } }|]
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Bun (DevEnginesField $ projectRoot </> [relfile|package.json|])

      it "detects the pnpm lockfile"
        $ withProject
          [ ".git/HEAD" ~> "",
            "pnpm-lock.yaml" ~> ""
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Pnpm (LockfileOrWorkspaceFile $ projectRoot </> [relfile|pnpm-lock.yaml|])

      it "detects the yarn lockfile"
        $ withProject
          [ ".git/HEAD" ~> "",
            "yarn.lock" ~> ""
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Yarn (LockfileOrWorkspaceFile $ projectRoot </> [relfile|yarn.lock|])

      it "detects the bun lockfile"
        $ withProject
          [ ".git/HEAD" ~> "",
            "bun.lock" ~> ""
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Bun (LockfileOrWorkspaceFile $ projectRoot </> [relfile|bun.lock|])

      it "detects the npm lockfile"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package-lock.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Npm (LockfileOrWorkspaceFile $ projectRoot </> [relfile|package-lock.json|])

      it "detects install metadata"
        $ withProject
          [ ".git/HEAD" ~> "",
            "node_modules/.modules.yaml" ~> ""
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Pnpm (InstallMetadata $ projectRoot </> [relfile|node_modules/.modules.yaml|])

      it "prefers the packageManager field over the devEngines field"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package.json"
              ~> [trimming|
                {
                  "devEngines": { "packageManager": { "name": "bun" } },
                  "packageManager": "pnpm@10.20.0"
                }
              |]
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Pnpm (PackageManagerField $ projectRoot </> [relfile|package.json|])

      it "prefers the devEngines field over the lockfile"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package.json" ~> [trimming|{ "devEngines": { "packageManager": { "name": "bun" } } }|],
            "package-lock.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Bun (DevEnginesField $ projectRoot </> [relfile|package.json|])

      it "prefers pnpm-lock.yaml over a stray package-lock.json"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package-lock.json" ~> "{}",
            "pnpm-lock.yaml" ~> ""
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Pnpm (LockfileOrWorkspaceFile $ projectRoot </> [relfile|pnpm-lock.yaml|])

      it "prefers the lockfile over install metadata"
        $ withProject
          [ ".git/HEAD" ~> "",
            "node_modules/.modules.yaml" ~> "",
            "package-lock.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Npm (LockfileOrWorkspaceFile $ projectRoot </> [relfile|package-lock.json|])

      it "ignores an invalid package.json"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package.json" ~> "{ not json",
            "bun.lock" ~> ""
          ]
        $ \projectRoot ->
          detectFromProjectFiles projectRoot
            `shouldReturn` found Bun (LockfileOrWorkspaceFile $ projectRoot </> [relfile|bun.lock|])

    describe "in the workspace roots above the project directory" $ do
      it "finds the pnpm workspace root's lockfile from a package"
        $ withProject
          [ ".git/HEAD" ~> "",
            "pnpm-workspace.yaml" ~> "",
            "pnpm-lock.yaml" ~> "",
            "apps/web/package.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles (projectRoot </> [reldir|apps/web|])
            `shouldReturn` found Pnpm (LockfileOrWorkspaceFile $ projectRoot </> [relfile|pnpm-lock.yaml|])

      it "finds the npm workspace root's lockfile from a package"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package.json" ~> [trimming|{ "workspaces": ["apps/*"] }|],
            "package-lock.json" ~> "{}",
            "apps/web/package.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles (projectRoot </> [reldir|apps/web|])
            `shouldReturn` found Npm (LockfileOrWorkspaceFile $ projectRoot </> [relfile|package-lock.json|])

      it "finds the workspace root declared with the object form"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package.json" ~> [trimming|{ "workspaces": { "packages": ["apps/*"] } }|],
            "bun.lock" ~> "",
            "apps/web/package.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles (projectRoot </> [reldir|apps/web|])
            `shouldReturn` found Bun (LockfileOrWorkspaceFile $ projectRoot </> [relfile|bun.lock|])

      it "keeps going up when a workspace root says nothing"
        $ withProject
          [ ".git/HEAD" ~> "",
            "pnpm-workspace.yaml" ~> "",
            "pnpm-lock.yaml" ~> "",
            "nested/package.json" ~> [trimming|{ "workspaces": ["apps/*"] }|],
            "nested/apps/web/package.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles (projectRoot </> [reldir|nested/apps/web|])
            `shouldReturn` found Pnpm (LockfileOrWorkspaceFile $ projectRoot </> [relfile|pnpm-lock.yaml|])

      it "lets the nearest project win"
        $ withProject
          [ ".git/HEAD" ~> "",
            "pnpm-workspace.yaml" ~> "",
            "apps/web/package-lock.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles (projectRoot </> [reldir|apps/web|])
            `shouldReturn` found Npm (LockfileOrWorkspaceFile $ projectRoot </> [relfile|apps/web/package-lock.json|])

      it "ignores a lockfile in an ancestor that isn't a workspace root"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package.json" ~> "{}",
            "package-lock.json" ~> "{}",
            "apps/web/package.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles (projectRoot </> [reldir|apps/web|]) `shouldReturn` foundNothing

      it "ignores the packageManager field in an ancestor that isn't a workspace root"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package.json" ~> [trimming|{ "packageManager": "pnpm@10.20.0" }|],
            "apps/web/package.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles (projectRoot </> [reldir|apps/web|]) `shouldReturn` foundNothing

      it "ignores an invalid package.json in a workspace root"
        $ withProject
          [ ".git/HEAD" ~> "",
            "pnpm-workspace.yaml" ~> "",
            "pnpm-lock.yaml" ~> "",
            "package.json" ~> "{ not json",
            "apps/web/package.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles (projectRoot </> [reldir|apps/web|])
            `shouldReturn` found Pnpm (LockfileOrWorkspaceFile $ projectRoot </> [relfile|pnpm-lock.yaml|])

      it "ignores an invalid package.json in an ancestor"
        $ withProject
          [ ".git/HEAD" ~> "",
            "package.json" ~> "{ not json",
            "apps/web/package.json" ~> "{}"
          ]
        $ \projectRoot ->
          detectFromProjectFiles (projectRoot </> [reldir|apps/web|]) `shouldReturn` foundNothing

      it "stops at the repository root"
        $ withProject
          [ "pnpm-workspace.yaml" ~> "",
            "pnpm-lock.yaml" ~> "",
            "repo/.git/HEAD" ~> ""
          ]
        $ \projectRoot ->
          detectFromProjectFiles (projectRoot </> [reldir|repo|]) `shouldReturn` foundNothing

      it "treats a .git file, as in git worktrees, as the repository root"
        $ withProject
          [ "pnpm-workspace.yaml" ~> "",
            "pnpm-lock.yaml" ~> "",
            "worktree/.git" ~> "gitdir: /somewhere/else"
          ]
        $ \projectRoot ->
          detectFromProjectFiles (projectRoot </> [reldir|worktree|]) `shouldReturn` foundNothing
  where
    found packageManager source = Just $ Detection packageManager source
    foundNothing = Nothing

-- | A file in the project, relative to the project root, with its contents.
(~>) :: FilePath -> Text -> (FilePath, Text)
(~>) = (,)

-- | Runs a check against a temporary project made of the given files. The
-- check gets the project root.
withProject :: [(FilePath, Text)] -> (Path' Abs (Dir PackageRootDir) -> IO ()) -> IO ()
withProject files check =
  withSystemTempDirectory "wasp-package-manager" $
    canonicalizePath >=> \projectRoot -> do
      forM_ files $ \(file, content) -> do
        createDirectoryIfMissing True $ FP.takeDirectory $ projectRoot FP.</> file
        T.writeFile (projectRoot FP.</> file) content
      check $ fromJust $ SP.parseAbsDir projectRoot
