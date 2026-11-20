import { isBuiltin } from "node:module";
import { defineConfig } from "tsdown";

export default defineConfig({
  entry: {
    index: "./src/index.ts",
    browser: "./src/browser/index.ts",
    node: "./src/node/index.ts",
    // Keep Vite's runtime dependency out of the general Node.js entry.
    vite: "./src/node/vite/index.ts",
  },
  platform: "neutral",
  deps: {
    // The "neutral" platform doesn't know about Node.js built-ins, and warns
    // about them being unresolved. The Node.js entries do use them.
    neverBundle: (id) => isBuiltin(id),
  },
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
