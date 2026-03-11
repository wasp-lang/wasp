import react, { type Options as ReactOptions } from "@vitejs/plugin-react";
import { type PluginOption } from "vite";
import { detectServerImports } from "./detectServerImports.js";
import { envFile } from "./envFile.js";
import { waspHtmlBuild } from "./html/build.js";
import { waspHtmlDev } from "./html/dev.js";
import { typescriptCheck } from "./typescriptCheck.js";
import { waspVirtualModules } from "./virtualModules.js";
import { waspConfig } from "./waspConfig.js";

export interface WaspPluginOptions {
  reactOptions?: ReactOptions;
}

export function wasp(options?: WaspPluginOptions): PluginOption {
  return [
    /**
    * Plugins running before core plugins (enforce: 'pre').
    */
    // The `wasp:config` plugin must come first because
    // other plugins may depend on its configuration.
    waspConfig(),
    waspVirtualModules(),
    envFile(),
    detectServerImports(),
    /**
     * Plugins running after core Vite plugins.
     */
    typescriptCheck(),
    waspHtmlDev(),
    waspHtmlBuild(),
    react(options?.reactOptions),
  ];
}
