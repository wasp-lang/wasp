import { readFileSync } from "node:fs";
import * as path from "node:path";

/**
 * What Wasp generates from the app for the Vite plugin. Written by
 * `Wasp.Generator.SdkGenerator.Client.VitePluginG`, so keep the two in sync.
 */
export type GeneratedViteConfig = {
  baseDir: string;
  clientBuildDir: string;
  clientPortEnvVarName: string;
  clientEnvFileName: string;
  clientEnvSchemaValidationModulePath: string;
  srcDir: string;
  srcTsConfigPath: string;
  clientEntryPointPath: string;
  routesEntryPointPath: string;
  ssrEntryPointPath: string;
  ssrPaths: string[];
  spaFallbackFile: string;
  depsExcludedFromOptimization: string[];
  /**
   * Maps the ID of a virtual user module to the user's module, relative to
   * the project dir.
   *
   * @example
   * virtualUserModules["virtual:wasp/user/env"] // => "./src/env"
   */
  virtualUserModules: Record<string, string>;
};

const generatedViteDirInProjectDir = ".wasp/out/sdk/wasp/client/vite";
const generatedViteConfigFileName = "config.json";
const virtualFilesDirInGeneratedViteDir = "virtual-files/files";

export function getGeneratedViteDir(projectDir: string): string {
  return path.resolve(projectDir, generatedViteDirInProjectDir);
}

export function readGeneratedViteConfig(
  generatedViteDir: string,
): GeneratedViteConfig {
  const configPath = path.join(generatedViteDir, generatedViteConfigFileName);
  try {
    return JSON.parse(readFileSync(configPath, "utf-8"));
  } catch (error) {
    throw new Error(
      `Couldn't read ${configPath}. Wasp generates it when it compiles your app, so run \`wasp compile\` first.`,
      { cause: error },
    );
  }
}

/** Reads a generated virtual file, e.g. the one for `/@wasp/routes.tsx`. */
export function readGeneratedVirtualFile(
  generatedViteDir: string,
  virtualFileId: string,
): string {
  return readFileSync(
    path.join(
      generatedViteDir,
      virtualFilesDirInGeneratedViteDir,
      path.basename(virtualFileId),
    ),
    "utf-8",
  );
}
