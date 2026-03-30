import assert from "node:assert/strict";
import { isRunnableDevEnvironment, type Plugin } from "vite";
import type { PrerenderFn } from "../../types";
import { ENVIRONMENT_NAMES, PACKAGE_NAME } from "../common/constants";
import type { Options } from "../common/options";
import type { SsrRoutes } from "../common/routes";
import { appendCssRegistrationCode, withRegisteredCss } from "./css";

export const ssrDev = (
  routes: SsrRoutes,
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
          return appendCssRegistrationCode(id, code);
        }
      },
    },

    configureServer(server) {
      // Returning a function here means that we want our middlewares to run
      // _after_ Vite's own middlewares.
      return () => {
        const ssrEnv = server.environments[ENVIRONMENT_NAMES.SSR];
        assert(
          isRunnableDevEnvironment(ssrEnv),
          "Expected ssr to be a runnable dev environment",
        );

        server.middlewares.use(async ({ url, originalUrl }, res, next) => {
          /*
            This middleware will run after Vite's own middlewares, so they have
            already handled requests for static assets or virtual helpers.

            By this point, if the request was for any built-in path or static
            asset, Vite has already handled it. Now Vite is looking for the
            `index.html`, which is the SPA page it serves for all non-asset
            requests.

            So, even if the user requested `/some-page`, Vite has already
            rewritten it to `/index.html`. With `req.url` we get `/index.html`
            so we know we're looking for the HTML file; but `req.originalUrl` is
            still `/some-page`, and we can pass that to our prerendering logic.
          */
          if (!url || url !== "/index.html" || !originalUrl) {
            return next();
          }

          const route = routes.byPath.has(originalUrl)
            ? originalUrl
            : routes.spaFallbackFile.path;

          // Clear the SSR module cache on every request, so that we always run the latest code.
          ssrEnv.runner.clearCache();

          const html = await withRegisteredCss(ssrEnv.hot, async () => {
            const { default: prerenderApp }: { default: PrerenderFn } =
              await ssrEnv.runner.import(ssrEntrySrc);
            return await prerenderApp(route, { clientEntrySrc });
          });

          if (!html) {
            return next();
          }

          const newHtml = await server.transformIndexHtml(originalUrl, html);

          res.statusCode = 200;
          res.setHeader("Content-Type", "text/html");
          res.end(newHtml);
        });
      };
    },
  };
};
