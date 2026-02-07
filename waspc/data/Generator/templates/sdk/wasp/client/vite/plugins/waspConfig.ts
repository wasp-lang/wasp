{{={= =}=}}
import { type PluginOption, mergeConfig } from "vite";
import { defaultExclude } from "vitest/config"

export function waspConfig(): PluginOption {
  // Track whether this is an SSR build so the transform hook can apply
  // browser-detection replacements only in the SSR bundle.
  let isSsr = false;

  return {
    name: "wasp:config",
    enforce: 'pre',
    config(config) {
      isSsr = !!config.build?.ssr;

      return mergeConfig({
        base: "{= baseDir =}",
        optimizeDeps: {
          exclude: {=& depsExcludedFromOptimization =}
        },
        server: {
          port: {= defaultClientPort =},
          host: "0.0.0.0",
          open: true,
        },
        envPrefix: "REACT_APP_",
        build: {
          outDir: "{= clientBuildDirPath =}",
        },
        resolve: {
          // These packages rely on a single instance per page. Not deduping them
          // causes runtime errors (e.g., hook rule violation in react, QueryClient
          // instance error in react-query, Invariant Error in react-router).
          dedupe: ["react", "react-dom", "@tanstack/react-query", "react-router"],
          alias: [
            {
              // Vite doesn't look for `.prisma/client` imports in the `node_modules`
              // folder. We point it to the correct place here.
              // TODO: Check if we can remove when updating Prisma (#2504)
              find: /^\.prisma\/client\/(.+)$/,
              replacement: "node_modules/.prisma/client/$1.js",
            },
            {
              // Handle bare .prisma/client import
              find: /^\.prisma\/client$/,
              replacement: "node_modules/.prisma/client",
            },
          ],
        },
        ssr: {
          // Bundle all dependencies into the SSR output instead of leaving them
          // as bare external imports for Node.js to resolve at runtime. This
          // prevents ESM resolution errors (ERR_MODULE_NOT_FOUND,
          // ERR_UNSUPPORTED_DIR_IMPORT) from browser-only packages that don't
          // have proper Node.js ESM exports (e.g., monaco-editor, react-icons).
          // Node.js built-ins (fs, path, http, etc.) are always kept external.
          noExternal: true,
          // Prisma Client uses __dirname and native query engine binaries.
          // It must remain external (resolved by Node.js at runtime, not bundled).
          external: ['@prisma/client', '.prisma/client'],
        },
        test: {
          globals: true,
          environment: "jsdom",
          setupFiles: {=& vitest.setupFilesArray =},
          exclude: [
            ...defaultExclude,
            "{= vitest.excludeWaspArtefactsPattern =}",
          ]
        },
      }, config);
    },

    // During the SSR build, replace `typeof document` and `typeof window`
    // with `"undefined"` so that browser-detection guards in Emotion, React,
    // MUI, and other libraries correctly take the server code path.
    //
    // Without this, Wasp's runtime polyfills (in server-ssr.mjs) make
    // `typeof document !== "undefined"` evaluate to `true`, which forces
    // Emotion into its browser path where useInsertionEffect (a no-op in
    // renderToString) handles style injection — resulting in zero CSS in
    // the SSR HTML.
    //
    // We use a `transform` hook instead of Vite's `define` option because
    // esbuild (used by Vite internally) does not support `typeof x` as a
    // define key.  The Rollup transform pipeline handles it correctly.
    //
    // The runtime polyfills remain as a safety net for code that accesses
    // these globals *without* a typeof guard (e.g. `document.createElement`).
    transform(code, id) {
      if (!isSsr) return null;
      // Only transform JS/TS source files (id may contain ?query params)
      if (!/\.[cm]?[jt]sx?(\?|$)/.test(id)) return null;

      let changed = false;
      // Use alternation to match string literals first (and skip them),
      // then match `typeof document` / `typeof window` in actual code.
      // This prevents replacing typeof checks that appear inside string
      // literals (e.g. papaparse embeds code in strings like
      //   "if (typeof window !== 'undefined') ..."
      // where a naive replacement would break the quoted syntax).
      const result = code.replace(
        /("(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|`(?:[^`\\]|\\.)*`)|typeof\s+(document|window)\b/gs,
        (match, stringLiteral) => {
          if (stringLiteral) return match; // preserve string literals
          changed = true;
          return '"undefined"';
        }
      );

      if (!changed) return null;
      return { code: result, map: null };
    },
  };
}
