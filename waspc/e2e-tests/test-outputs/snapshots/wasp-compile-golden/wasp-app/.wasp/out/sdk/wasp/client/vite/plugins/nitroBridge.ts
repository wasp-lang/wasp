import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { type Plugin } from "vite";
// Teaches Vite's `UserConfig` about the `nitro` key we return below.
import type {} from "nitro/vite";
import { getVirtualFileAbsPath } from "../virtual-files/resolver.js";

const clientEntryPointPath = "/@wasp/client-entry.tsx";
const ssrEntryPointPath = "/@wasp/ssr-entry.tsx";

/**
 * The generated server's Nitro entry point, relative to the Wasp project
 * directory (Vite's root). It hands the requests for the app's API over to
 * Wasp's Express app, and lets everything else through to the renderer.
 */
const serverEntryPointPath = ".wasp/out/server/src/nitro/serverEntry.ts";

/**
 * The path Nitro's renderer is served from. It is a real file in the SDK, not
 * one of our virtual files, because Nitro bundles it twice: once with Vite (for
 * the server it builds) and once with a bare Rollup/Rolldown build that knows
 * nothing about our plugins (for prerendering). See `nitroRenderer.ts`.
 */
const rendererPath = fileURLToPath(new URL("../nitroRenderer.js", import.meta.url));

/**
 * Translates the Wasp app's configuration into the Vite environments and the
 * Nitro configuration that build and serve the client.
 *
 * It must come before `nitro()` in the plugin array: Vite feeds each plugin's
 * `config()` result into the config object the next plugin sees, and Nitro
 * reads its own configuration from the `nitro` key there.
 */
export function waspNitroBridge(): Plugin {
  return {
    name: "wasp:nitro-bridge",
    config(config, { command }) {
      const rootDir = path.resolve(config.root ?? ".");

      return {
        environments: {
          // Nitro doesn't fall back to an `index.html`, so it needs to be told
          // explicitly where the browser app starts.
          client: {
            build: {
              rollupOptions: {
                input: getVirtualFileAbsPath(rootDir, clientEntryPointPath),
              },
            },
          },
          // Everything the renderer needs (React, the routes, the asset tags)
          // lives in the `ssr` environment, so that it goes through Vite.
          ssr: {
            build: {
              rollupOptions: {
                input: getVirtualFileAbsPath(rootDir, ssrEntryPointPath),
              },
            },
            resolve: {
              // The SDK is a symlinked workspace package, which Vite bundles
              // instead of externalizing. We say so explicitly because
              // externalizing it would make Node load its CSS imports.
              noExternal: ["wasp"],
            },
          },
        },
        nitro: {
          preset: "node-server",

          // Nitro serves the app from the same subdirectory Vite builds it for.
          // Vite's `base` alone isn't enough, these are separate options.
          baseURL: "/",

          renderer: { handler: rendererPath },

          serverEntry: {
            handler: path.resolve(rootDir, serverEntryPointPath),
            // Nitro would otherwise run the handler through a Node-style
            // adapter that can't tell "I didn't handle this request" apart from
            // an empty response, which makes the renderer unreachable.
            format: "web",
          },

          // Off by default, but we say so explicitly: left to auto-detection,
          // Nitro would treat directories that happen to follow its conventions
          // (`routes/`, `api/`, `middleware/`, `plugins/`, `tasks/`) as server
          // code and let them shadow the app's pages. It also shadows the
          // server entry we set above.
          serverDir: false,

          prerender: {
            routes: [
              ...[],
              // The SPA shell. Static hosts serve it for any path they don't
              // have a prerendered file for.
              "/200.html",
            ],
            // Wasp's list of prerendered paths is authoritative, we don't want
            // Nitro discovering more of them by following links.
            crawlLinks: false,
            failOnError: true,
          },

          // Only when building. In dev, Nitro keeps its output in a scratch
          // directory inside its build cache, and we want to leave it there:
          // it serves everything in its public output directory as a static
          // file, so pointing it at the build output would make the dev server
          // serve (and choke on) the files of the last build.
          ...(command === "build"
            ? {
                output: {
                  dir: ".wasp/out/web-app/",
                  publicDir: ".wasp/out/web-app/build/",
                },
              }
            : {}),
        },
      };
    },
  };
}
