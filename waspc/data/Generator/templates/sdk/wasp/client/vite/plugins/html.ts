{{={= =}=}}
import { type Plugin } from "vite";
import { getIndexHtmlContent } from "./virtual-files/indexHtml.virtual.js";

export function waspHtml(): Plugin {
  return {
    name: "wasp-html",
    config() {
      return {
        appType: "custom",
        // build: {
        //   rollupOptions: {
        //     input: "virtual:wasp/index",
        //   },
        // },
      };
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
      const html = getIndexHtmlContent().replace(
        '<script type="module" src="{= clientEntryPointPath =}"></script>',
        `<script type="module" src="/${entryFileName}"></script>`
      );

      this.emitFile({
        type: "asset",
        fileName: "index.html",
        source: html,
      });
    },
  };
}
