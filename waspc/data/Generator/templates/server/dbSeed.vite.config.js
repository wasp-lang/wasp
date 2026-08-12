import { virtualUserModules } from "./src/plugins/virtualUserModules.js";

/**
 * Bundles the database seeding script (`src/dbSeed.ts`) into something Node can
 * run, which is what `wasp db seed` does before running it.
 *
 * The script needs bundling for the same reason the rest of your server's code
 * does: the code Wasp generates for you (the `wasp` package) is TypeScript, and
 * imports its own files without extensions, neither of which Node can load.
 *
 * Everything else is left alone: your app's dependencies are installed next to
 * it, and Node resolves them when the seed runs.
 */
export default {
  plugins: [virtualUserModules()],
  ssr: {
    // The `wasp` package is a symlinked workspace package, so Vite treats it as
    // an installed dependency and would leave it to Node otherwise.
    noExternal: ["wasp"],
  },
  build: {
    ssr: "src/dbSeed.ts",
    outDir: "dist",
    emptyOutDir: true,
    target: "esnext",
    minify: false,
    sourcemap: true,
    rollupOptions: {
      output: { format: "es", entryFileNames: "dbSeed.js" },
    },
  },
};
