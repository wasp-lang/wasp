{{={= =}=}}
/// <reference types="vitest" />
import { mergeConfig } from "vite";
import react from "@vitejs/plugin-react";
import { defaultExclude } from "vitest/config"
import { detectServerImports } from "./vite/detectServerImports"
import { validateEnv } from "./vite/validateEnv.js";
import path from "node:path"

{=# customViteConfig.isDefined =}
// Ignoring the TS error because we are importing a file outside of TS root dir.
// @ts-ignore
{=& customViteConfig.importStatement =}
const _waspUserProvidedConfig = {=& customViteConfig.importIdentifier =}
{=/ customViteConfig.isDefined =}
{=^ customViteConfig.isDefined =}
const _waspUserProvidedConfig = {};
{=/ customViteConfig.isDefined =}

const defaultViteConfig = {
  base: "{= baseDir =}",
  plugins: [
    validateEnv(),
    react(),
    detectServerImports(),
  ],
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
    outDir: "build",
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
        find: /^\.prisma\/(.+)$/,
        replacement: path.join(
          "{= projectDir =}",
          "node_modules/.prisma/$1"
        ),
      },
    ],
  },
  test: {
    globals: true,
    environment: "jsdom",
    // Since Vitest is running from the root of the project, we need
    // to specify the path to the setup file relative to the root.
    setupFiles: {=& vitest.setupFilesArray =},
    exclude: [
      ...defaultExclude,
      "{= vitest.excludeWaspArtefactsPattern =}",
    ]
  },
};

// https://vitejs.dev/config/
export default mergeConfig(
  defaultViteConfig,
  _waspUserProvidedConfig
);
