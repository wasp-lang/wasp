/// <reference types="vitest" />
import { mergeConfig } from "vite";
import react from "@vitejs/plugin-react";
import { defaultExclude } from "vitest/config"

// Ignoring the TS error because we are importing a file outside of TS root dir.
// @ts-ignore
import customViteConfig from '../../../vite.config'
const _waspUserProvidedConfig = customViteConfig

const defaultViteConfig = {
  base: "/",
  plugins: [react()],
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
    setupFiles: ['.wasp/out/web-app/src/test/vitest/setup.ts'],
    exclude: [
      ...defaultExclude,
      ".wasp/**/*",
    ]
  },
};

// https://vitejs.dev/config/
export default mergeConfig(
  defaultViteConfig,
  _waspUserProvidedConfig
);
