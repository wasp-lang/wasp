import assert from "node:assert/strict";
import { isRunnableDevEnvironment, type Plugin } from "vite";
import { PLUGIN_NAME, type Options } from "./common";
import type { Routes } from "./routes";
import type { PrerenderFn } from "./types";

export const REGISTER_CSS_EVENT_NAME = `${PLUGIN_NAME}:register-css` as const;
export type RegisterCssData = { id: string };

declare module "vite/types/customEvent.d.ts" {
  interface CustomEventMap {
    [REGISTER_CSS_EVENT_NAME]: RegisterCssData;
  }
}

export const ssrDev = (
  routes: Routes,
  { ssrEntrySrc, clientEntrySrc }: Options,
): Plugin => {
  return {
    name: `${PLUGIN_NAME}:dev`,
    apply: "serve",

    transform: {
      order: "post",
      filter: { id: /\.css$/ },
      async handler(code, id, { ssr = false } = {}) {
        if (!ssr) {
          return;
        }

        const newCode = `${code}
if (import.meta.hot) {
  import.meta.hot.send(${JSON.stringify(REGISTER_CSS_EVENT_NAME)}, { id: ${JSON.stringify(id)} });
}
`;

        return newCode;
      },
    },

    configureServer:
      (server) =>
      // Returning a function here means that we want our middlewares to run
      // _after_ Vite's own middlewares.
      () => {
        const ssrEnv = server.environments.ssr;
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

          ssrEnv.runner.clearCache();

          const cssFilesSet = new Set<string>();
          const onRegisterCss = ({ id }: RegisterCssData) =>
            cssFilesSet.add(id);
          ssrEnv.hot.on(REGISTER_CSS_EVENT_NAME, onRegisterCss);

          const { default: prerenderApp }: { default: PrerenderFn } =
            await ssrEnv.runner.import(ssrEntrySrc);

          const prerenderedResponse = await prerenderApp(route, {
            clientEntrySrc,
            transformIndexHtml: (html) =>
              server.transformIndexHtml(originalUrl, html),
          });

          ssrEnv.hot.off(REGISTER_CSS_EVENT_NAME, onRegisterCss);

          if (!prerenderedResponse) {
            return next();
          }

          assert(
            prerenderedResponse.ok &&
              prerenderedResponse.body &&
              prerenderedResponse.headers
                .get("Content-Type")
                ?.startsWith("text/html"),
            "Expected prerenderApp to return a successful response with an HTML body",
          );

          const html = await prerenderedResponse.text();
          const newHtml = html.replace(/<\/head>/, () => {
            const cssLinks = Array.from(cssFilesSet)
              .map((id) => `<link rel="stylesheet" href="${id}">`)
              .join("");
            return `${cssLinks}</head>`;
          });

          res.statusCode = 200;
          res.setHeader("Content-Type", "text/html");
          res.end(newHtml);
        });
      },
  };
};
