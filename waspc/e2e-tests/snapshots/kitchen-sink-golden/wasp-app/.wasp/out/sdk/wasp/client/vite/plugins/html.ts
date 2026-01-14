import type { Plugin, Connect } from "vite";
import path from "node:path";
import { getIndexHtmlContent } from "./virtual-files/indexHtml.virtual.js";

const indexHtmlFileName = "index.html";
let indexHtmlPath: string;

export function waspHtml(): Plugin {
  return {
    name: "wasp-html",
    config() {
      return {
        appType: "custom",
        build: {
          rollupOptions: {
            input: indexHtmlFileName,
          },
        },
      };
    },
    configResolved(config) {
      indexHtmlPath = path.resolve(config.root, indexHtmlFileName);
    },
    resolveId(id) {
      // Resolve index.html to a virtual module
      if (id === indexHtmlFileName) {
        return indexHtmlPath;
      }
    },
    load(id) {
      // Provide content for virtual index.html
      if (id === indexHtmlPath) {
        return getIndexHtmlContent();
      }
    },
    configureServer(server) {
      server.middlewares.use(spaFallbackMiddleware());

      return () => {
        server.middlewares.use(async (req, res, next) => {
          if (req.url === "/" || req.url === `/${indexHtmlFileName}`) {
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
    configurePreviewServer(server) {
      server.middlewares.use(spaFallbackMiddleware());
    }
  };
}

function spaFallbackMiddleware(): Connect.NextHandleFunction {
    return (req, _res, next) => {
      if (
        (req.method === "GET" || req.method === "HEAD") &&
        req.url !== "/favicon.ico" &&
        (req.headers.accept === undefined ||
          req.headers.accept === "" ||
          req.headers.accept.includes("text/html") ||
          req.headers.accept.includes("*/*"))
      ) {
        const url = req.url || "/";
        // If it's a route (no extension, not a special Vite path), rewrite to /index.html
        if (!url.includes(".") && !url.startsWith("/@")) {
          req.url = `/${indexHtmlFileName}`;
        }
      }
      next();
    };
  }
