import { sendResponse } from "@mjackson/node-fetch-server";
import assert from "node:assert/strict";
import { isRunnableDevEnvironment, type Plugin } from "vite";
import { PLUGIN_NAME, type Options } from "./common";
import type { Routes } from "./routes";
import type { PrerenderFn } from "./types";

export const ssrDev = (
  routes: Routes,
  { ssrEntrySrc, clientEntrySrc }: Options,
): Plugin => {
  return {
    name: `${PLUGIN_NAME}:dev`,
    apply: "serve",

    configureServer:
      (server) =>
      // Returning a function here means that we want our middlewares to run
      // _after_ Vite's own middlewares.
      () => {
        const ssrEnv = server.environments.ssr;
        assert(
          isRunnableDevEnvironment(ssrEnv),
          "Expected ssr to be in a runnable dev environment",
        );

        server.middlewares.use(async ({ url, originalUrl }, res, next) => {
          /*
            This middleware will run after Vite's own middlewares, so they have
            already handled requests for static assets or virtual helpers.

            Only the requests for the SPA handler should reach this middleware,
            and we can identify them by checking if `req.url === "/index.html"`.

            `req.originalUrl` has the original URL before Vite's middlewares
            potentially rewrite it, so we can prerender the correct route.
          */

          if (!url || url !== "/index.html" || !originalUrl) {
            return next();
          }

          const route = routes.byPath.has(originalUrl)
            ? originalUrl
            : routes.fallback.path;

          const { default: prerenderApp }: { default: PrerenderFn } =
            await ssrEnv.runner.import(ssrEntrySrc);

          const prerenderedResponse = await prerenderApp(route, {
            clientEntrySrc,
            transformIndexHtml: (html) =>
              server.transformIndexHtml(originalUrl, html),
          });

          if (!prerenderedResponse) {
            return next();
          }

          await sendResponse(res, prerenderedResponse);
        });
      },
  };
};
