import assert from "node:assert/strict";
import { isRunnableDevEnvironment, type Plugin } from "vite";
import type { PrerenderFn } from "../../types";
import { ENVIRONMENT_NAMES, PACKAGE_NAME } from "../common/constants";
import type { Options } from "../common/options";
import type { Routes } from "../common/routes";
import { appendToHead } from "../util/html";
import { addRegisterCss, collectRegisteredCss } from "./css";

export const ssrDev = (
  routes: Routes,
  { ssrEntrySrc, clientEntrySrc }: Options,
): Plugin => {
  return {
    name: `${PACKAGE_NAME}:dev`,
    apply: "serve",

    transform: {
      filter: { id: /\.css$/ },

      // We're asking to go _after_ Vite's own CSS handling, which converts CSS
      // imports into JS modules that inject styles at runtime. We want to run
      // after that so we can register the final CSS file paths.
      order: "post",

      handler(code, id, opts) {
        if (opts?.ssr) {
          return addRegisterCss(id, code);
        }
      },
    },

    configureServer:
      (server) =>
      // Returning a function here means that we want our middlewares to run
      // _after_ Vite's own middlewares.
      () => {
        const ssrEnv = server.environments[ENVIRONMENT_NAMES.SSR];
        assert(
          isRunnableDevEnvironment(ssrEnv),
          "Expected ssr to be a runnable dev environment",
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

          // Clear the SSR module cache on every request, so that we always run the latest code.
          ssrEnv.runner.clearCache();

          const { result: html, cssFiles } = await collectRegisteredCss(
            ssrEnv.hot,
            async () => {
              const { default: prerenderApp }: { default: PrerenderFn } =
                await ssrEnv.runner.import(ssrEntrySrc);

              return await prerenderApp(route, {
                clientEntrySrc,
                transformIndexHtml: (html) =>
                  server.transformIndexHtml(originalUrl, html),
              });
            },
          );

          if (!html) {
            return next();
          }

          const newHtml = appendToHead(
            html,
            Array.from(cssFiles)
              .map((id) => `<link rel="stylesheet" href="${id}">\n`)
              .join(""),
          );

          res.statusCode = 200;
          res.setHeader("Content-Type", "text/html");
          res.end(newHtml);
        });
      },
  };
};
