import assert from "node:assert/strict";
import * as path from "node:path";
import type { Plugin } from "vite";
import { PLUGIN_NAME, type Options } from "./common";
import type { Routes } from "./routes";
import type { PrerenderFn } from "./types";

export const ssrBuild = (
  routes: Routes,
  { ssrEntrySrc, clientEntrySrc }: Options,
): Plugin => {
  let prerenderApp: PrerenderFn | null = null;

  return {
    name: `${PLUGIN_NAME}:build`,
    apply: "build",
    sharedDuringBuild: true,

    config() {
      return {
        environments: {
          ssr: {
            build: {
              ssr: true,
              rollupOptions: { input: ssrEntrySrc },
            },
          },
          client: {
            build: {
              rollupOptions: {
                input: Array.from(routes.byId.keys()),
              },
            },
          },
        },

        builder: {
          async buildApp(builder) {
            const ssrEnv = builder.environments.ssr;
            const clientEnv = builder.environments.client;

            const ssrOutput = await builder.build(ssrEnv);
            assert(
              !Array.isArray(ssrOutput),
              "Expected ssr build output to be a single chunk",
            );
            assert(
              "output" in ssrOutput,
              "Watch mode is not supported for ssr production builds",
            );

            const entryChunk = ssrOutput.output[0];
            assert(
              entryChunk,
              "Expected ssr build output to have at least one chunk",
            );
            assert(
              entryChunk.exports.includes("default"),
              "Expected ssr build output chunk to export a default export",
            );

            const ssrPath = path.resolve(
              ssrEnv.config.build.outDir,
              entryChunk.fileName,
            );

            prerenderApp = (await import(ssrPath)).default;

            await builder.build(clientEnv);
          },
        },
      };
    },

    resolveId: {
      filter: { id: /\.html$/ },
      handler(id, _, { ssr }) {
        if (ssr) return;

        assert(
          prerenderApp,
          "Expected ssr build to have completed before resolving ids in client build",
        );

        if (routes.byId.has(id)) {
          return id;
        }
      },
    },

    load: {
      filter: { id: /\.html$/ },
      async handler(id, { ssr = false } = {}) {
        if (ssr) return;

        assert(
          prerenderApp,
          "Expected ssr build to have completed before resolving ids in client build",
        );

        const route = routes.byId.get(id);
        assert(route, `Unexpected id ${id} not found in ssrRoutes`);

        const prerenderedAppResponse = await prerenderApp(route.path, {
          clientEntrySrc,
          transformIndexHtml: async (html) => html,
        });

        if (!prerenderedAppResponse) return;

        assert(
          prerenderedAppResponse.ok &&
            prerenderedAppResponse.body &&
            prerenderedAppResponse.headers
              .get("Content-Type")
              ?.startsWith("text/html"),
          "Expected prerenderApp to return a successful response with an HTML body",
        );

        return await prerenderedAppResponse.text();
      },
    },
  };
};
