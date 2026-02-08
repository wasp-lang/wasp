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
      // Scan the source to replace `typeof document` / `typeof window`
      // with `"undefined"`, but only in code context — never inside
      // string literals or template-literal quasi (text) sections.
      //
      // A character-level scanner is used instead of a single regex so
      // that template expressions (`${...}`) are correctly treated as
      // code (while quasi text is still skipped).
      const typeofRe = /typeof\s+(document|window)\b/y;
      const tmplStack: number[] = [];
      let braceDepth = 0;
      let result = '';
      let pos = 0;

      while (pos < code.length) {
        const ch = code[pos];

        // Single- or double-quoted string — copy verbatim
        if (ch === "'" || ch === '"') {
          let j = pos + 1;
          while (j < code.length && code[j] !== ch) {
            if (code[j] === '\\') j++;
            j++;
          }
          result += code.slice(pos, j + 1);
          pos = j + 1;
          continue;
        }

        // Template literal — copy quasi text verbatim, recurse into exprs
        if (ch === '`') {
          result += '`';
          pos = scanQuasi(pos + 1);
          continue;
        }

        // Closing } of a template expression — resume quasi scanning
        if (ch === '}' && tmplStack.length > 0 && braceDepth === 0) {
          result += '}';
          braceDepth = tmplStack.pop()!;
          pos = scanQuasi(pos + 1);
          continue;
        }

        // Brace tracking inside template expressions
        if (ch === '{') { braceDepth++; result += '{'; pos++; continue; }
        if (ch === '}') { braceDepth--; result += '}'; pos++; continue; }

        // Line comment — copy verbatim
        if (ch === '/' && code[pos + 1] === '/') {
          const nl = code.indexOf('\n', pos);
          const end = nl < 0 ? code.length : nl;
          result += code.slice(pos, end);
          pos = end;
          continue;
        }

        // Block comment — copy verbatim
        if (ch === '/' && code[pos + 1] === '*') {
          const close = code.indexOf('*/', pos + 2);
          const end = close < 0 ? code.length : close + 2;
          result += code.slice(pos, end);
          pos = end;
          continue;
        }

        // typeof document / typeof window — replace in code context
        if (ch === 't') {
          typeofRe.lastIndex = pos;
          const m = typeofRe.exec(code);
          if (m) {
            result += '"undefined"';
            changed = true;
            pos += m[0].length;
            continue;
          }
        }

        result += ch;
        pos++;
      }

      if (!changed) return null;
      return { code: result, map: null };

      /**
       * Scan a template-literal quasi section (the literal text between
       * back-ticks / `${` / `}`).  Copies characters verbatim into
       * `result` and returns the new scan position.  Stops at:
       *   - closing backtick (included, returns position after it)
       *   - `${` (included, pushes braceDepth onto tmplStack, returns
       *     position after `{` so the main loop processes the expression)
       */
      function scanQuasi(from: number): number {
        let j = from;
        while (j < code.length) {
          if (code[j] === '\\') {
            result += code.slice(j, j + 2);
            j += 2;
            continue;
          }
          if (code[j] === '`') {
            result += '`';
            return j + 1;
          }
          if (code[j] === '$' && code[j + 1] === '{') {
            result += '${';
            tmplStack.push(braceDepth);
            braceDepth = 0;
            return j + 2;
          }
          result += code[j];
          j++;
        }
        return j;
      }
    },
  };
}
