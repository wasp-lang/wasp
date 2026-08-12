{{={= =}=}}
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { type Plugin } from "vite";
// Teaches Vite's `UserConfig` about the `nitro` key we return below.
import type {} from "nitro/vite";
import { getVirtualFileAbsPath } from "../virtual-files/resolver.js";

const clientEntryPointPath = "{= clientEntryPointPath =}";
const ssrEntryPointPath = "{= ssrEntryPointPath =}";

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
          baseURL: "{= baseDir =}",

          renderer: { handler: rendererPath },

          // We don't have any server code running through Nitro yet (the Wasp
          // server is still its own Express process). Both of these are off by
          // default, but we say so explicitly: left to auto-detection, Nitro
          // would pick up a `server.ts` lying around in the project, or treat
          // directories that happen to follow its conventions (`routes/`,
          // `api/`, `middleware/`, `plugins/`, `tasks/`) as server code and
          // let them shadow the app's pages.
          serverEntry: false,
          serverDir: false,

          prerender: {
            routes: [
              ...{=& prerenderPaths =},
              // The SPA shell. Static hosts serve it for any path they don't
              // have a prerendered file for.
              "{= spaFallbackFilePath =}",
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
                  dir: "{= nitroOutputDirPath =}",
                  publicDir: "{= clientBuildDirPath =}",
                },
              }
            : {}),
        },
      };
    },
  };
}
