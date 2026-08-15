import { defineConfig } from "tsdown";

export default defineConfig({
  entry: ["src/index.ts", "src/client.ts"],
  outDir: "dist",
  clean: true,

  platform: "neutral",
  target: "node22",
  format: "esm",

  sourcemap: true,
  dts: { sourcemap: true },
});
