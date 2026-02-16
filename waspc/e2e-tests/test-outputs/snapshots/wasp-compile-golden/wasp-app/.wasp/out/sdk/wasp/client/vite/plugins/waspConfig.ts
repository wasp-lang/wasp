import { type PluginOption, mergeConfig } from "vite";
import { defaultExclude } from "vitest/config"

export function waspConfig(): PluginOption {
  return {
    name: "wasp:config",
    enforce: 'pre',
    config(config) {
      return mergeConfig({
        base: "/",
        optimizeDeps: {
          exclude: ['wasp', '@wasp.sh/lib-auth']
        },
        server: {
          port: 3000,
          host: "0.0.0.0",
          open: true,
        },
        envPrefix: "REACT_APP_",
        build: {
          outDir: ".wasp/out/web-app/build/",
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
        test: {
          globals: true,
          environment: "jsdom",
          setupFiles: ['wasp/client/test/setup'],
          exclude: [
            ...defaultExclude,
            ".wasp/**/*",
          ]
        },
      }, config);
    }
  };
}
