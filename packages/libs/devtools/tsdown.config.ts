import { defineConfig } from "tsdown";

export default defineConfig([
  {
    entry: ["src/node.ts"],
    outDir: "dist",
    clean: true,

    platform: "node",
    target: "node22",
    format: "esm",

    sourcemap: true,
    dts: { sourcemap: true },
  },
  {
    // The toolbar runs in the app's page, prebuilt with its dependencies.
    entry: { toolbar: "src/toolbar/client.ts" },
    outDir: "dist/client",
    clean: false,

    platform: "browser",
    target: "es2022",
    format: "esm",
    noExternal: () => true,
    minify: true,
  },
]);
