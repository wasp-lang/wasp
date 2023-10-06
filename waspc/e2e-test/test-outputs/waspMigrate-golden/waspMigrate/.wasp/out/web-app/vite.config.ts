/// <reference types="vitest" />
import { mergeConfig } from "vite";
import react from "@vitejs/plugin-react-swc";

const _waspUserProvidedConfig = {};

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
