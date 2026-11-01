import { defineConfig } from "tsdown";

export default defineConfig({
  entry: {
    index: "./src/index.ts",
    // Keep the testing libraries out of the config entry, which vite.config.ts
    // imports.
    config: "./src/config.ts",
  },
  platform: "neutral",
  unbundle: true,
  outDir: "dist",
  dts: { sourcemap: true },
  sourcemap: true,
  fixedExtension: true,
  attw: { profile: "esm-only", level: "error" },
});
