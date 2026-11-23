import { defineConfig } from "tsdown";

export default defineConfig({
  entry: ["src/node.ts"],
  outDir: "dist",
  clean: true,

  platform: "node",
  target: "node22",
  format: "esm",
  // Inlines imported images, like the Wasp logo, into the bundle.
  loader: { ".svg": "dataurl" },

  sourcemap: true,
  dts: { sourcemap: true },
});
