import { defineConfig } from "tsdown";

export default defineConfig({
  entry: { node: "./src/node.ts" },
  platform: "node",
  outDir: "dist",
  dts: { sourcemap: true },
  sourcemap: true,
  fixedExtension: true,
  attw: { profile: "esm-only", level: "error" },
});
