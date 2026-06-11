module Tests.SnapshotTests.WaspBuildSnapshotTest (waspBuildSnapshotTest) where

import Command (Command, cmd, withEnvVars)
import Context (WaspProjectContext (..))
import qualified Data.Text as T
import NeatInterpolation (trimming)
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import Step (Step, askStepContext)
import Steps
  ( buildAndRemoveWaspProjectDockerImage,
    createSnapshotWaspProjectFromMinimalStarter,
    inSnapshotWaspProjectDir,
    runCommand,
    setWaspDbToPSQL,
    waspCliBuild,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import qualified StrongPath.FilePath as FP
import Wasp.Project.Common (WaspProjectDir)

waspBuildSnapshotTest :: SnapshotTest
waspBuildSnapshotTest =
  makeSnapshotTest "wasp-build" $ do
    createSnapshotWaspProjectFromMinimalStarter
    inSnapshotWaspProjectDir $ do
      setWaspDbToPSQL
      runCommand waspCliBuild
      buildAndRemoveWaspProjectDockerImage
      wrapViteConfigForDeterministicBuild
      runCommand viteBuild

-- | Renames the generated vite.config.ts and wraps it with a config that adds
-- deterministic build options (no minification, no hashes, externalized deps),
-- so the snapshot output is stable and easy to diff.
wrapViteConfigForDeterministicBuild :: Step WaspProjectContext ()
wrapViteConfigForDeterministicBuild = do
  context <- askStepContext

  writeToFile
    (context.waspProjectDir </> wrapperViteFile)
    [trimming|
      import { mergeConfig, type Plugin } from "vite";
      import originalConfig from "${importOriginalFromMain}";

      export default mergeConfig(originalConfig, {
        plugins: [externalizeNodeModules()],
        build: {
          // Keep output readable for easier snapshot diffing.
          minify: false,
          rollupOptions: {
            output: {
              // Strip content hashes for deterministic filenames across runs.
              entryFileNames: "assets/[name].js",
              chunkFileNames: "assets/[name].js",
              assetFileNames: "assets/[name].[ext]",
            },
          },
        },
      });

      // Externalize any import that resolves to node_modules,
      // so the build output only contains app code, for cleaner diffs.
      function externalizeNodeModules(): Plugin {
        return {
          name: "externalize-node-modules",
          enforce: "pre",
          async resolveId(source, importer, options) {
            if (!importer) return null;
            const resolved = await this.resolve(source, importer, {
              ...options,
              skipSelf: true,
            });
            if (resolved && resolved.id.includes("/node_modules/")) {
              // We externalize the module
              return { id: source, external: true };
            } else {
              // We let resolution proceed as normal
              return null;
            }
          },
        };
      }
    |]
  where
    importOriginalFromMain :: T.Text
    importOriginalFromMain = T.pack $ "./" ++ FP.fromRelFile originalViteFile

viteBuild :: Command
viteBuild =
  withEnvVars [("REACT_APP_API_URL", "http://localhost:3001")] $
    cmd "npx" ["vite", "build", "--config", FP.fromRelFile wrapperViteFile]

originalViteFile :: SP.Path' (SP.Rel WaspProjectDir) SP.File'
originalViteFile = [relfile|vite.config.ts|]

wrapperViteFile :: SP.Path' (SP.Rel WaspProjectDir) SP.File'
wrapperViteFile = [relfile|vite.config.wrapper.ts|]
