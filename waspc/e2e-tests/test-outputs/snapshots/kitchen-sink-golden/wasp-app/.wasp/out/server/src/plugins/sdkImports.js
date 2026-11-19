import { getSdkDir, readSdkExports, resolveSdkImport } from "@wasp/sdk-core/node/sdk";
import path from "node:path";

const generatedAppDir = path.resolve(import.meta.dirname, "..", "..", "..");

/**
 * Resolves `wasp/*` imports to the generated SDK, which isn't installed in
 * `node_modules`. The SDK's files end up in the bundle, because its relative
 * imports have no file extensions, so only a bundler can resolve them (see
 * https://github.com/wasp-lang/wasp/issues/2492).
 *
 * @returns {import("rollup").Plugin}
 */
export function sdkImports() {
  const sdkDir = getSdkDir(generatedAppDir);
  const sdkExports = readSdkExports(sdkDir);

  return {
    name: "wasp:sdk-imports",
    resolveId(source, importer, options) {
      const sdkFilePath = resolveSdkImport(sdkDir, sdkExports, source);
      return sdkFilePath === undefined
        ? null
        : this.resolve(sdkFilePath, importer, { ...options, skipSelf: true });
    },
  };
}
