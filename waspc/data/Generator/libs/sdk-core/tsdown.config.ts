import { defineConfig } from "tsdown";

export default defineConfig({
  entry: {
    index: "./src/index.ts",
    browser: "./src/browser/index.ts",
    node: "./src/node/index.ts",
    // Keep Vite's runtime dependency out of the general Node.js entry.
    vite: "./src/node/vite/index.ts",
    // Keep Testing Library's runtime dependency out of the general browser entry.
    test: "./src/browser/test/index.ts",
  },
  platform: "neutral",
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
  fixedExtension: false,
});
