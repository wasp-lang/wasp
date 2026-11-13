import { readFile } from "node:fs/promises";
import { basename } from "node:path";

import type { Plugin, ResolvedConfig } from "vite";

const TOOLBAR_BASE = "/__wasp-devtools/";
const TOOLBAR_PATH = `${TOOLBAR_BASE}toolbar.js`;

/**
 * Adds the Wasp DevTools toolbar to the app's page during development. It
 * replaces the floating dock of Vite DevTools (`waspDevTools()` hides that
 * one), and shows the same panels. Does nothing unless the project enables
 * Vite DevTools.
 */
export function waspToolbar(): Plugin {
  return {
    name: "wasp:devtools-toolbar",
    apply: "serve",
    // These hooks read the config of the server they run in instead of keeping
    // it from `configResolved`, since Wasp also resolves the plugins for
    // temporary servers that have Vite DevTools disabled.
    configureServer(server) {
      if (!isDevToolsEnabled(server.config)) return;
      // The toolbar is prebuilt, so it stays out of the app's module graph,
      // like the Vite DevTools dock it replaces.
      server.middlewares.use(TOOLBAR_BASE, async (req, res, next) => {
        // `basename` keeps requests inside the toolbar's build directory.
        const fileName = basename(new URL(req.url ?? "", "http://x").pathname);
        if (!fileName.endsWith(".js")) return next();
        try {
          const script = await readFile(
            new URL(`./client/${fileName}`, import.meta.url),
          );
          res.setHeader("Content-Type", "text/javascript");
          res.setHeader("Cache-Control", "no-cache");
          res.end(script);
        } catch {
          next();
        }
      });
    },
    transformIndexHtml: {
      // Like Vite DevTools' own injection, so it runs before Wasp renders the
      // page.
      order: "pre",
      handler(_html, { server }) {
        if (!server || !isDevToolsEnabled(server.config)) return;
        return [
          {
            tag: "script",
            attrs: { type: "module", src: TOOLBAR_PATH },
            injectTo: "body",
          },
        ];
      },
    },
  };
}

function isDevToolsEnabled(config: ResolvedConfig): boolean {
  return Boolean(config.devtools && config.devtools.enabled);
}
