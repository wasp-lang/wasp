import { createPluginFromDevframe } from "@vitejs/devtools-kit/node";
import type { Plugin } from "vite";

import { createWaspStudioDevframe, type WaspStudioOptions } from "./devframe";

export type { WaspStudioOptions } from "./devframe";

/**
 * Adds Wasp Studio as a panel to Vite DevTools. The panel only shows up when
 * the project enables Vite DevTools (`devtools` in its Vite config).
 */
export function waspStudio(options: WaspStudioOptions): Plugin {
  return {
    ...createPluginFromDevframe(createWaspStudioDevframe(options), {
      // Listed first, with the framework's own tools.
      dock: { category: "framework" },
    }),
    apply: "serve",
  };
}
