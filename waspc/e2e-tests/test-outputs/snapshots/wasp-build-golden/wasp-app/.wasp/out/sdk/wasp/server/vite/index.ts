import { type PluginOption } from "vite";
import { detectClientImports } from "./plugins/detectClientImports.js";
import { virtualUserModules } from "./plugins/virtualUserModules.js";
import { waspServerConfig } from "./plugins/waspServerConfig.js";

/**
 * Declares the Wasp server as a Vite environment named `server`.
 *
 * Wasp bundles the production server through this environment, which is why
 * this plugin must be present in the project's `vite.config.ts`.
 */
export function waspServer(): PluginOption {
  return [
    // The `wasp:server-config` plugin must come first because it declares the
    // `server` environment the other plugins attach to.
    waspServerConfig(),
    virtualUserModules(),
    detectClientImports(),
  ];
}
