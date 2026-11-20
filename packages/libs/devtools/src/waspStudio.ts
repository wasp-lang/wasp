import { fileURLToPath } from "node:url";

import type { Plugin } from "vite";

// The lib's build copies the Wasp Studio page here, next to this module's
// bundle.
const STUDIO_PAGE_DIR = fileURLToPath(new URL("./studio", import.meta.url));

const STUDIO_PAGE_BASE = "/__wasp-studio/";

/**
 * Adds Wasp Studio as a panel to Vite DevTools. The Vite dev server serves the
 * Wasp Studio page, which reads the app's spec from the app's server.
 */
export function waspStudio(): Plugin {
  return {
    name: "wasp:devtools:wasp-studio",
    apply: "serve",
    devtools: {
      setup(ctx) {
        ctx.views.hostStatic(STUDIO_PAGE_BASE, STUDIO_PAGE_DIR);
        ctx.docks.register({
          id: "wasp-studio",
          title: "Wasp Studio",
          icon: "ph:graph-duotone",
          type: "iframe",
          url: getStudioPageUrl(),
          // Listed first, with the framework's own tools.
          category: "framework",
        });
      },
    },
  };
}

/**
 * The Wasp Studio page reads the app's spec from the app's server, so we tell
 * it where that is. `wasp start` passes the server URL to the Vite process.
 */
function getStudioPageUrl(): string {
  const serverUrl = process.env["REACT_APP_API_URL"];
  return serverUrl
    ? `${STUDIO_PAGE_BASE}?${new URLSearchParams({ serverUrl })}`
    : STUDIO_PAGE_BASE;
}
