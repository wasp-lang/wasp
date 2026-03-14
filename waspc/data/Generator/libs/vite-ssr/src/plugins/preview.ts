import * as fs from "node:fs/promises";
import * as path from "node:path";
import type { Plugin } from "vite";
import { PACKAGE_NAME } from "./common/constants";
import type { Routes } from "./common/routes";

export const ssrPreview = (routes: Routes): Plugin => {
  return {
    name: `${PACKAGE_NAME}:preview`,

    configurePreviewServer: (server) => () => {
      const clientOutDir = server.config.build.outDir;

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

        const route = routes.byPath.get(originalUrl) ?? routes.fallback;

        const htmlPath = path.resolve(clientOutDir, route.id);
        if (!(await pathExists(htmlPath))) {
          return next();
        }

        const html = await fs.readFile(htmlPath, "utf-8");

        res.statusCode = 200;
        res.setHeader("Content-Type", "text/html");
        res.end(html);
      });
    },
  };
};

async function pathExists(...params: Parameters<typeof fs.access>) {
  try {
    await fs.access(...params);
    return true;
  } catch {
    return false;
  }
}
