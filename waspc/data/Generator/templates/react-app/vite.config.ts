{{={= =}=}}
/// <reference types="vitest" />
import { mergeConfig } from "vite";
import react from "@vitejs/plugin-react";
import { defaultExclude } from "vitest/config"

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
  plugins: [react()],
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
    // These packages rely on a single instance per page. Not dedpuing them
    // causes runtime errors (e.g., hook rule violation in react, QueryClient
    // instance error in react-query, Invariant Error in react-router-dom).
    dedupe: ["react", "react-dom", "@tanstack/react-query", "react-router-dom"]
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
