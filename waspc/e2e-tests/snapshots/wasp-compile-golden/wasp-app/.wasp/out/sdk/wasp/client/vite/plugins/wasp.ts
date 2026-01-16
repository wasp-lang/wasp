import { type Plugin, mergeConfig } from "vite";
import { defaultExclude } from "vitest/config"
import react, { type Options as ReactOptions } from "@vitejs/plugin-react";
import { validateEnv } from "./validateEnv.js";
import { detectServerImports } from "./detectServerImports.js";
import { waspVirtualModules } from "./virtualModules.js";
import { waspHtml } from "./html.js";

export interface WaspPluginOptions {
  reactOptions?: ReactOptions;
}

export function wasp(options?: WaspPluginOptions): Plugin[] {
  return [
    waspHtml(),
    waspVirtualModules(),
    validateEnv(),
    ...react(options?.reactOptions),
    detectServerImports(),
    {
      name: "wasp-config",
      config(config) {
        return mergeConfig({
          base: "/",
          optimizeDeps: {
            exclude: ['wasp']
          },
          server: {
            port: 3000,
            host: "0.0.0.0",
            open: true,
          },
          envPrefix: "REACT_APP_",
          build: {
            // TODO: decide if we want to keep the old build dir or introduce
            // a new one at root and use just `build`. This would affect the deployment,
            // but people would need to migrate to `npx vite build` anyways since there
            // is no longer a `build` script in `web-app` they can call.
            outDir: ".wasp/out/web-app/build",
          },
          resolve: {
            // These packages rely on a single instance per page. Not deduping them
            // causes runtime errors (e.g., hook rule violation in react, QueryClient
            // instance error in react-query, Invariant Error in react-router-dom).
            dedupe: ["react", "react-dom", "@tanstack/react-query", "react-router-dom"],
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
    },
  ];
}
