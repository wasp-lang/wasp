{{={= =}=}}
import type { PluginOption } from "vite";
import react from "@vitejs/plugin-react";
import {
  detectServerImports,
  envFile,
  typescriptCheck,
  validateEnv,
  type WaspPluginOptions,
} from "@wasp.sh/lib-sdk-core/node/vite";
import ssr from "@wasp.sh/lib-vite-ssr";
import { virtualWaspModules } from "./virtualWaspModules.js";
import { virtualUserModules } from "./virtualUserModules.js";
import { waspConfig } from "./waspConfig.js";



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
    envFile("{= clientEnvFileName =}"),
    detectServerImports("{= srcDirInWaspProjectDir =}"),
    /**
     * Plugins running after core Vite plugins.
     */
    typescriptCheck({ srcTsConfigPath: "{= srcTsConfigPath =}" }),
    validateEnv("{= clientEnvSchemaValidationModulePath =}"),
    react(options?.reactOptions),
    ssr({
      clientEntrySrc: "{= clientEntryPointPath =}",
      ssrEntrySrc: "{= ssrEntryPointPath =}",
      ssrPaths: {=& ssrPaths =},
      spaFallbackFile: "{= spaFallbackFile =}",
    }),
  ];
}
