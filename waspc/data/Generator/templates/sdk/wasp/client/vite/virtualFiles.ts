{{={= =}=}}
import path from "node:path";
import { type Plugin } from "vite";
import { getIndexTsxContent } from "./virtual-files/index.generated.js";
import { getRoutesTsxContent } from "./virtual-files/routes.generated.js";
import { getIndexHtmlContent } from "./virtual-files/indexHtml.js";

const indexTsxFileName = "{= indexTsxFileName =}";
const routesFileName = "{= routesFileName =}";

export function virtualFiles(): Plugin {
  let projectRoot: string;
  let indexTsxPath: string;
  let routesPath: string;

  return {
    name: "wasp-virtual-files",
    enforce: "pre",
    config(config) {
      // Set appType to 'custom' to prevent Vite from requiring index.html
      // We generate our own HTML in the generateBundle hook
      return {
        appType: "custom",
        build: {
          rollupOptions: {
            // Use virtual index.tsx as entry point instead of index.html
            input: path.resolve(config.root || process.cwd(), indexTsxFileName),
          },
        },
      };
    },
    configResolved(config) {
      projectRoot = config.root;
      // Using absolute paths gives proper context for resolving relative imports.
      indexTsxPath = path.resolve(projectRoot, indexTsxFileName);
      routesPath = path.resolve(projectRoot, routesFileName);
    },
    resolveId(id) {
      // Intercept requests for /src/index.tsx and resolve to virtual module
      // Also resolve the absolute path (used by build.rollupOptions.input)
      if (id === `/${indexTsxFileName}` || id === indexTsxPath) {
        return indexTsxPath;
      }
      // Intercept requests for routes.generated.tsx
      if (id === `./${routesFileName}`) {
        return routesPath;
      }
    },
    load(id) {
      if (id === indexTsxPath) {
        return {
          code: getIndexTsxContent(),
          map: null,
        };
      }
      if (id === routesPath) {
        return {
          code: getRoutesTsxContent(),
          map: null,
        };
      }
    },
    configureServer(server) {
      // Stage 1: SPA fallback - rewrite URLs (runs early, mimics Vite's htmlFallbackMiddleware)
      server.middlewares.use((req, _res, next) => {
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
            req.url = "/index.html";
          }
        }
        next();
      });

      // Stage 2: Serve transformed index.html (runs after Vite's middleware)
      return () => {
        server.middlewares.use(async (req, res, next) => {
          if (req.url === "/" || req.url === "/index.html") {
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
    generateBundle(_options, bundle) {
      // Find the entry chunk to get the actual output filename
      let entryFileName: string | undefined;
      for (const [fileName, chunk] of Object.entries(bundle)) {
        if (chunk.type === "chunk" && chunk.isEntry) {
          entryFileName = fileName;
          break;
        }
      }

      if (!entryFileName) {
        throw new Error("Could not find entry chunk in bundle");
      }

      // Generate index.html with the correct script reference
      const baseHtml = getIndexHtmlContent();
      // Replace the dev script src with the built script path
      const html = baseHtml.replace(
        `<script type="module" src="/${indexTsxFileName}"></script>`,
        `<script type="module" src="/${entryFileName}"></script>`
      );

      // Emit the HTML file
      this.emitFile({
        type: "asset",
        fileName: "index.html",
        source: html,
      });
    },
  };
}
