import type { Plugin } from "vite";
import { readSdkExports, resolveSdkImport } from "../../sdk/index.js";

/**
 * Resolves `wasp/*` imports to the generated SDK. The SDK isn't installed in
 * `node_modules`, so Vite can't find it on its own.
 */
export function sdkImports(sdkDir: string): Plugin {
  const sdkExports = readSdkExports(sdkDir);

  return {
    name: "wasp:sdk-imports",
    enforce: "pre",
    resolveId(source, importer, options) {
      const sdkFilePath = resolveSdkImport(sdkDir, sdkExports, source);
      return sdkFilePath === undefined
        ? null
        : this.resolve(sdkFilePath, importer, { ...options, skipSelf: true });
    },
  };
}
