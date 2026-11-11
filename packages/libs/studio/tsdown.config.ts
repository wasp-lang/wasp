import { defineConfig } from "tsdown";

export default defineConfig({
  entry: ["src/node.ts", "src/standalone.ts"],
  outDir: "dist",
  clean: true,

  platform: "node",
  target: "node22",
  format: "esm",

  sourcemap: true,
  dts: { sourcemap: true },
});
