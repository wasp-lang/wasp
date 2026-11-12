import createTerminalsDevframe from "@devframes/plugin-terminals";
import { createPluginFromDevframe } from "@vitejs/devtools-kit/node";
import type { Plugin } from "vite";

/**
 * Adds the Terminals panel to Vite DevTools, which shows the output of the
 * processes the other panels start (like `wasp studio`). Vite DevTools has its
 * own, but `waspDevTools()` turns off its built-in panels.
 */
export function waspTerminals(): Plugin {
  return {
    ...createPluginFromDevframe(createTerminalsDevframe(), {
      dock: { category: "advanced" },
    }),
    apply: "serve",
  };
}
