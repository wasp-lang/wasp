import { defineConfig } from "tsdown";

export default defineConfig({
  entry: ["src/index.ts", "src/types.ts"],
  outDir: "dist",
  clean: true,

  platform: "node",
  target: "node22",
  format: "esm",

  sourcemap: true,
  dts: { sourcemap: true },
});
