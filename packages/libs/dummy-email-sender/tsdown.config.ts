import { defineConfig } from "tsdown";

export default defineConfig([
  {
    entry: { node: "./src/node.ts" },
    platform: "node",
    outDir: "dist",
    dts: { sourcemap: true },
    sourcemap: true,
    fixedExtension: true,
    attw: { profile: "esm-only", level: "error" },
  },
  {
    // The inbox page, served from `dist/view` by `registerDevMailboxRoutes`.
    entry: { inbox: "./src/view/inbox.ts" },
    platform: "browser",
    outDir: "dist/view",
    dts: false,
    clean: false,
    // Bundles `inbox.css` and its imports into one file.
    css: { fileName: "inbox.css" },
    copy: ["./src/view/index.html"],
  },
]);
