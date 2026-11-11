import { DevTools } from "@vitejs/devtools";
import { createPluginFromDevframe } from "@vitejs/devtools-kit/node";
import type { Plugin, PluginOption } from "vite";

import { createWaspStudioDevframe, type WaspStudioOptions } from "./devframe";

export type { WaspStudioOptions } from "./devframe";

/**
 * Adds Vite DevTools to the dev server, with Wasp Studio as one of its panels.
 * Other Vite plugins can add their own panels through the `devtools` hook.
 */
export function waspStudio(options: WaspStudioOptions): PluginOption {
  // Vitest starts a Vite server too, but there is no page to show DevTools on.
  if (process.env.VITEST) {
    return [];
  }

  return [
    DevTools().then(onlyInDev),
    onlyInDev([createPluginFromDevframe(createWaspStudioDevframe(options))]),
  ];
}

function onlyInDev(plugins: Plugin[]): Plugin[] {
  return plugins.map((plugin) => ({ ...plugin, apply: "serve" }));
}
