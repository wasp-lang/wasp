import { mergeConfig, type Plugin } from "vite";
import originalConfig from "./vite.config.original.ts";

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