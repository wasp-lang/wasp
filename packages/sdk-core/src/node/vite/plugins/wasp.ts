import react, { type Options as ReactOptions } from "@vitejs/plugin-react";
import ssr from "@wasp/vite-ssr";
import type { PluginOption } from "vite";
import { getGeneratedAppDir, getSdkDir } from "../../sdk/index.js";
import {
  getGeneratedViteDir,
  readGeneratedViteConfig,
} from "../generatedViteConfig.js";
import { detectServerImports } from "./detectServerImports.js";
import { envFile } from "./envFile.js";
import { externalWaspDependencies } from "./externalWaspDependencies.js";
import { sdkImports } from "./sdkImports.js";
import { typescriptCheck } from "./typescriptCheck.js";
import { validateEnv } from "./validateEnv.js";
import { virtualUserModules } from "./virtualUserModules.js";
import { virtualWaspModules } from "./virtualWaspModules.js";
import { waspConfig } from "./waspConfig.js";

export interface WaspPluginOptions {
  reactOptions?: ReactOptions;
}

/**
 * The Vite plugin every Wasp app needs. It reads what Wasp generated from the
 * app, so Vite has to run from the project dir.
 */
export function wasp(options?: WaspPluginOptions): PluginOption {
  const generatedAppDir = getGeneratedAppDir(process.cwd());
  const sdkDir = getSdkDir(generatedAppDir);
  const generatedViteDir = getGeneratedViteDir(sdkDir);
  const generatedConfig = readGeneratedViteConfig(generatedViteDir);

  return [
    /**
     * Plugins running before core plugins (enforce: 'pre').
     */
    // The `wasp:config` plugin must come first because
    // other plugins may depend on its configuration.
    waspConfig(generatedConfig),
    sdkImports(sdkDir),
    externalWaspDependencies(generatedAppDir),
    virtualUserModules(generatedConfig.virtualUserModules),
    virtualWaspModules(generatedConfig, generatedViteDir),
    envFile(generatedConfig.clientEnvFileName),
    detectServerImports(generatedConfig.srcDir),
    /**
     * Plugins running after core Vite plugins.
     */
    typescriptCheck({ srcTsConfigPath: generatedConfig.srcTsConfigPath }),
    validateEnv(generatedConfig.clientEnvSchemaValidationModulePath),
    react(options?.reactOptions),
    ssr({
      clientEntrySrc: generatedConfig.clientEntryPointPath,
      ssrEntrySrc: generatedConfig.ssrEntryPointPath,
      ssrPaths: generatedConfig.ssrPaths,
      spaFallbackFile: generatedConfig.spaFallbackFile,
    }),
  ];
}
