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
      // Let Vite process @wasp.sh/lib-sdk-core so its component CSS is included in the snapshot.
      if (resolved?.id.includes("/node_modules/@wasp.sh/lib-sdk-core/")) return null;
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