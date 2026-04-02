{{={= =}=}}
import { type PluginOption } from "vite";
import react, { type Options as ReactOptions } from "@vitejs/plugin-react";
import ssr from "@wasp.sh/lib-vite-ssr";
import { envFile } from "./envFile.js";
// import { validateEnv } from "./validateEnv.js";
import { detectServerImports } from "./detectServerImports.js";
import { typescriptCheck } from "./typescriptCheck.js";
import { userVirtualModules } from "./userVirtualModules.js";
import { waspConfig } from "./waspConfig.js";
import { waspVirtualModules } from "./waspVirtualModules.js";

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
    userVirtualModules(),
    waspVirtualModules(),
    envFile(),
    detectServerImports(),
    /**
     * Plugins running after core Vite plugins.
     */
    typescriptCheck(),
    // NOTE: temporary, untill we resolve https://github.com/wasp-lang/wasp/issues/3875
    // validateEnv(),
    react(options?.reactOptions),
    ssr({
      clientEntrySrc: "{= clientEntryPointPath =}",
      ssrEntrySrc: "{= ssrEntryPointPath =}",
      ssrPaths: [],
      ssrFallbackFile: "{= ssrFallbackFile =}",
    }),
  ];
}
