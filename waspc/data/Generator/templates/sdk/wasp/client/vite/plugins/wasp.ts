{{={= =}=}}
import path from "node:path"
import { type PluginOption, mergeConfig } from "vite";
import { defaultExclude } from "vitest/config"
import react, { type Options as ReactOptions } from "@vitejs/plugin-react";
import { validateEnv } from "./validateEnv.js";
import { detectServerImports } from "./detectServerImports.js";
import { waspVirtualModules } from "./virtualModules.js";
import { waspHtmlDev } from "./html/dev.js";
import { waspHtmlBuild } from "./html/build.js";
import { typescriptCheck } from "./typescriptCheck.js";

export interface WaspPluginOptions {
  reactOptions?: ReactOptions;
}

export function wasp(options?: WaspPluginOptions): PluginOption {
  return [
    typescriptCheck(),
    waspHtmlDev(),
    waspHtmlBuild(),
    waspVirtualModules(),
    validateEnv(),
    react(options?.reactOptions),
    detectServerImports(),
    {
      name: "wasp:config",
      config(config) {
        return mergeConfig({
          base: "{= baseDir =}",
          optimizeDeps: {
            exclude: ['wasp']
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
            setupFiles: {=& vitest.setupFilesArray =},
            exclude: [
              ...defaultExclude,
              "{= vitest.excludeWaspArtefactsPattern =}",
            ]
          },
        }, config);
      }
    },
  ];
}
