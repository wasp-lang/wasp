module Tests.SnapshotTests.WaspBuildSnapshotTest (waspBuildSnapshotTest) where

import E2e
import NeatInterpolation (trimming)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspBuildSnapshotTest :: E2eTest
waspBuildSnapshotTest =
  snapshotTest "wasp-build" $
    withNewWaspProject minimalStarterTemplate $ do
      setDbToPostgres
      wasp ["build"]
      buildAndRemoveDockerImage
      writeDeterministicViteConfig
      run . withEnv [("REACT_APP_API_URL", "http://localhost:3001")] $
        command "npx" ["vite", "build", "--config", deterministicViteConfigFile]

-- | Wraps the project's vite.config.ts with deterministic build options (no
-- minification, no hashes, externalized deps), so the snapshot is stable and
-- easy to diff.
writeDeterministicViteConfig :: TestM ()
writeDeterministicViteConfig =
  writeToFile
    deterministicViteConfigFile
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

deterministicViteConfigFile :: FilePath
deterministicViteConfigFile = "vite.config.wrapper.ts"
