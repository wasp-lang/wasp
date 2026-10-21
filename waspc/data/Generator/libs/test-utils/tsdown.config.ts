import { defineConfig } from "tsdown";

export default defineConfig({
  entry: { index: "./src/index.ts" },
  platform: "neutral",
  unbundle: true,
  outDir: "dist",
  dts: { sourcemap: true },
  sourcemap: true,
  fixedExtension: true,
  attw: { profile: "esm-only", level: "error" },
});
