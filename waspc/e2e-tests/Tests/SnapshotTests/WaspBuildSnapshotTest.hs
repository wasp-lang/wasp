module Tests.SnapshotTests.WaspBuildSnapshotTest (waspBuildSnapshotTest) where

import Control.Monad (void)
import E2E
import NeatInterpolation (trimming)
import qualified StrongPath as SP
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir)

waspBuildSnapshotTest :: E2ETest
waspBuildSnapshotTest =
  snapshotTest "wasp-build" $
    withNewWaspProject minimalStarterTemplate $ do
      setDbToPostgres
      wasp ["build"]
      buildAndRemoveDockerImage
      writeDeterministicViteConfig
      run
        $ withEnv [("REACT_APP_API_URL", "http://localhost:3001")]
        $ nodeModulesBin "vite" ["build", "--config", wrapperViteConfigFile]

-- | Builds the Docker image of the generated app, to check that its
-- Dockerfile works.
buildAndRemoveDockerImage :: TestM ()
buildAndRemoveDockerImage =
  whenDockerEnabled $
    inDirectory (SP.fromRelDir $ dotWaspDirInWaspProjectDir SP.</> generatedAppDirInDotWaspDir) $ do
      run $ command "docker" ["build", "--build-arg", "BUILDKIT_DOCKERFILE_CHECK=error=true", "-t", imageTag, "."]
      void . run $ command "docker" ["image", "rm", imageTag]
  where
    imageTag = "waspc-e2e-tests-" ++ waspAppDirName

-- | Wraps the generated vite.config.ts with a config that adds
-- deterministic build options (no minification, no hashes, externalized deps),
-- so the snapshot output is stable and easy to diff.
writeDeterministicViteConfig :: TestM ()
writeDeterministicViteConfig =
  writeToFile
    wrapperViteConfigFile
    [trimming|
      import { mergeConfig, type Plugin } from "vite";
      import originalConfig from "./vite.config.ts";

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

      // Externalize most JS dependencies to keep snapshot diffs small.
      function externalizeNodeModules(): Plugin {
        return {
          name: "externalize-node-modules",
          apply: "build",
          enforce: "pre",
          async resolveId(source, importer, options) {
            if (!importer) return null;
            const resolved = await this.resolve(source, importer, {
              ...options,
              skipSelf: true,
            });
            // Let Vite process @wasp/lib-sdk-core so its component CSS is included in the snapshot.
            if (resolved?.id.includes("/node_modules/@wasp/lib-sdk-core/")) return null;
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

wrapperViteConfigFile :: FilePath
wrapperViteConfigFile = "vite.config.wrapper.ts"
