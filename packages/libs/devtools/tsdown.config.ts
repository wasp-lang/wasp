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
    // The API Playground panel, served as its own page in an iframe.
    entry: { playground: "src/playground/app/main.tsx" },
    outDir: "dist/client",
    clean: false,

    platform: "browser",
    target: "es2022",
    format: "esm",
    noExternal: () => true,
    minify: true,
  },
]);
