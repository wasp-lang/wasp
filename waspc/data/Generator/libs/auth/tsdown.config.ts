import { defineConfig } from "tsdown";

export default defineConfig({
  entry: {
    index: "./src/shared/index.ts",
    browser: "./src/browser/index.ts",
    node: "./src/node/index.ts",
  },
  platform: "neutral",
  unbundle: true,
  css: {
    // Load component styles automatically when importing components.
    inject: true,
    lightningcss: {
      // Keep CSS hashes stable across checkout paths for snapshot tests.
      projectRoot: import.meta.dirname,
    },
  },
  outDir: "dist",
  dts: { sourcemap: true },
  sourcemap: true,
  fixedExtension: true,
  attw: { profile: "esm-only", level: "error" },
});
