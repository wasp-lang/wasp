import { defineConfig } from "tsdown";

export default defineConfig({
  entry: {
    index: "./src/index.ts",
    browser: "./src/browser/index.ts",
    node: "./src/node/index.ts",
  },
  platform: "neutral",
  outDir: "dist",
  dts: { sourcemap: true },
  sourcemap: true,
  fixedExtension: false,
});
