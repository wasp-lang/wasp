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
    // The toolbar runs in the app's page, served as one self-contained file.
    entry: { toolbar: "src/toolbar/client.ts" },
    outDir: "dist/client",
    clean: false,

    platform: "browser",
    target: "es2022",
    format: "esm",
    outExtensions: () => ({ js: ".js" }),
    noExternal: () => true,
    minify: true,
  },
]);
