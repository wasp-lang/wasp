{{={= =}=}}
/// <reference types="vitest" />
import { mergeConfig } from "vite";
import react from "@vitejs/plugin-react";

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
  test: {
    environment: "jsdom",
    setupFiles: ["./src/test/vitest/setup.ts"],
  },
  // resolve: {
  //   dedupe: ["react", "react-dom"],
  // },
};

// https://vitejs.dev/config/
export default mergeConfig(
  defaultViteConfig,
  _waspUserProvidedConfig
);
