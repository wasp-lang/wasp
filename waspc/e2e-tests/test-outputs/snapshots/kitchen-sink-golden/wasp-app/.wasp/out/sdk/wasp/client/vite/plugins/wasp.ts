import { type PluginOption } from "vite";
import react, { type Options as ReactOptions } from "@vitejs/plugin-react";
import { nitro } from "nitro/vite";
import { validateEnv } from "./validateEnv.js";
import { envFile } from "./envFile.js";
import { detectServerImports } from "./detectServerImports.js";
import { virtualWaspModules } from "./virtualWaspModules.js";
import { virtualUserModules } from "./virtualUserModules.js";
import { typescriptCheck } from "./typescriptCheck.js";
import { waspConfig } from "./waspConfig.js";
import { waspNitroBridge } from "./nitroBridge.js";

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
    virtualUserModules(),
    virtualWaspModules(),
    envFile(),
    detectServerImports(),
    /**
     * Plugins running after core Vite plugins.
     */
    typescriptCheck({ srcTsConfigPath: "tsconfig.src.json" }),
    validateEnv(),
    react(options?.reactOptions),
    /**
     * Nitro builds and serves the app. The bridge translates the Wasp app's
     * configuration into Nitro's, so it must come right before `nitro()`,
     * which must come last.
     */
    waspNitroBridge(),
    nitro(),
  ];
}
