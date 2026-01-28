import type { Plugin } from "vite";
import { getIndexHtmlContent } from "../../virtual-files/index.js";

export function waspHtmlDev(): Plugin {
  return {
    name: "wasp:html-dev",
    apply: "serve",
    configureServer(server) {
      return () => {
        // Post middleware: runs after Vite's built-in SPA fallback
        // middleware which resolves routes to `/index.html` which
        // we pick up here and return the virtual `index.html` content.
        server.middlewares.use(async (req, res, next) => {
          if (req.url === "/" || req.url === `/index.html`) {
            try {
              const html = getIndexHtmlContent();
              const transformedHtml = await server.transformIndexHtml(
                req.url,
                html
              );

              res.setHeader("Content-Type", "text/html");
              res.end(transformedHtml);
              return;
            } catch (e) {
              return next(e);
            }
          }

          next();
        });
      };
    },
  };
}
