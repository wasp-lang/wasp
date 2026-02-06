{{={= =}=}}
import { type PluginOption, mergeConfig } from "vite";
import { defaultExclude } from "vitest/config"

export function waspConfig(): PluginOption {
  return {
    name: "wasp:config",
    enforce: 'pre',
    config(config) {
      return mergeConfig({
        base: "{= baseDir =}",
        optimizeDeps: {
          exclude: {=& depsExcludedFromOptimization =}
        },
        server: {
          port: {= defaultClientPort =},
          host: "0.0.0.0",
          open: true,
        },
        envPrefix: "REACT_APP_",
        build: {
          outDir: "{= clientBuildDirPath =}",
        },
        resolve: {
          // These packages rely on a single instance per page. Not deduping them
          // causes runtime errors (e.g., hook rule violation in react, QueryClient
          // instance error in react-query, Invariant Error in react-router).
          dedupe: ["react", "react-dom", "@tanstack/react-query", "react-router"],
          alias: [
            {
              // Vite doesn't look for `.prisma/client` imports in the `node_modules`
              // folder. We point it to the correct place here.
              // TODO: Check if we can remove when updating Prisma (#2504)
              find: /^\.prisma\/client\/(.+)$/,
              replacement: "node_modules/.prisma/client/$1.js",
            },
            {
              // Handle bare .prisma/client import
              find: /^\.prisma\/client$/,
              replacement: "node_modules/.prisma/client",
            },
          ],
        },
        ssr: {
          // Bundle all dependencies into the SSR output instead of leaving them
          // as bare external imports for Node.js to resolve at runtime. This
          // prevents ESM resolution errors (ERR_MODULE_NOT_FOUND,
          // ERR_UNSUPPORTED_DIR_IMPORT) from browser-only packages that don't
          // have proper Node.js ESM exports (e.g., monaco-editor, react-icons).
          // Node.js built-ins (fs, path, http, etc.) are always kept external.
          noExternal: true,
          // Prisma Client uses __dirname and native query engine binaries.
          // It must remain external (resolved by Node.js at runtime, not bundled).
          external: ['@prisma/client', '.prisma/client'],
        },
        test: {
          globals: true,
          environment: "jsdom",
          setupFiles: {=& vitest.setupFilesArray =},
          exclude: [
            ...defaultExclude,
            "{= vitest.excludeWaspArtefactsPattern =}",
          ]
        },
      }, config);
    }
  };
}
