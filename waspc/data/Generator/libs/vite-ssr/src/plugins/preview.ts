import * as fs from "node:fs/promises";
import * as path from "node:path";
import type { Plugin } from "vite";
import { PACKAGE_NAME } from "./common/constants";
import type { SsrRoutes } from "./common/routes";
import { pathExists } from "./util/path";

export const ssrPreview = (routes: SsrRoutes): Plugin => {
  return {
    name: `${PACKAGE_NAME}:preview`,

    configurePreviewServer(server) {
      // Returning a function here means that we want our middlewares to run
      // _after_ Vite's own middlewares.
      return () => {
        const clientOutDir = server.config.build.outDir;

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

            Note that Vite rewrites `req.url` but not `req.originalUrl`, so the
            latter still carries the query string and the `base` prefix (if
            any); `routes.match` normalizes those away before looking up the
            route.
          */
          if (!url || url !== "/index.html" || !originalUrl) {
            return next();
          }

          const route = routes.match(originalUrl, server.config.base);

          const htmlPath = path.resolve(clientOutDir, route.id);
          if (!(await pathExists(htmlPath))) {
            return next();
          }

          const html = await fs.readFile(htmlPath, "utf-8");

          res.statusCode = 200;
          res.setHeader("Content-Type", "text/html");
          res.end(html);
        });
      };
    },
  };
};
