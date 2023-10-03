{{={= =}=}}
/// <reference types="vitest" />
import { mergeConfig } from "vite";
import react from "@vitejs/plugin-react-swc";

{=# isCustomViteConfigUsed =}
import _waspUserProvidedConfig from "./src/ext-src/vite.config";
{=/ isCustomViteConfigUsed =}
{=^ isCustomViteConfigUsed =}
const _waspUserProvidedConfig = {};
{=/ isCustomViteConfigUsed =}

const defaultViteConfig = {
  plugins: [react()],
  server: {
    port: 3000,
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
};

// https://vitejs.dev/config/
export default mergeConfig(
  defaultViteConfig,
  _waspUserProvidedConfig
);
