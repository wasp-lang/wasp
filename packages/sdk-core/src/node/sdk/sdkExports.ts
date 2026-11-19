import { readFileSync } from "node:fs";
import * as path from "node:path";

/**
 * The `exports` of the generated SDK's `package.json`: a subpath like
 * `./client/operations` or `./server/crud/*`, and the file it points at.
 */
export type SdkExports = Readonly<Record<string, string>>;

const sdkPackageName = "wasp";
const generatedAppDirInProjectDir = ".wasp/out";
const sdkDirInGeneratedAppDir = "sdk/wasp";

export function getGeneratedAppDir(projectDir: string): string {
  return path.resolve(projectDir, generatedAppDirInProjectDir);
}

export function getSdkDir(generatedAppDir: string): string {
  return path.join(generatedAppDir, sdkDirInGeneratedAppDir);
}

export function readSdkExports(sdkDir: string): SdkExports {
  const packageJsonPath = path.join(sdkDir, "package.json");
  try {
    return JSON.parse(readFileSync(packageJsonPath, "utf-8")).exports;
  } catch (error) {
    throw new Error(
      `Couldn't read ${packageJsonPath}. Wasp generates it when it compiles your app, so run \`wasp compile\` first.`,
      { cause: error },
    );
  }
}

/**
 * The import specifier of an SDK export, e.g. `wasp/client/operations` for
 * `./client/operations`.
 */
function getSdkImportSpecifier(exportSubpath: string): string {
  return path.posix.join(sdkPackageName, exportSubpath);
}

/**
 * TypeScript resolves the same imports through the tsconfig that Wasp writes
 * (see `Wasp.Generator.ImportAliases`).
 *
 * Returns the SDK file that an import like `wasp/client/operations` points at.
 * Returns `undefined` for imports of other packages, and for SDK files that
 * the SDK doesn't export.
 */
export function resolveSdkImport(
  sdkDir: string,
  sdkExports: SdkExports,
  importSpecifier: string,
): string | undefined {
  for (const [exportSubpath, exportTarget] of Object.entries(sdkExports)) {
    const wildcardMatch = matchExport(
      getSdkImportSpecifier(exportSubpath),
      importSpecifier,
    );
    if (wildcardMatch !== undefined) {
      return path.join(sdkDir, exportTarget.replace("*", wildcardMatch));
    }
  }
  return undefined;
}

/**
 * Returns what the `*` of the export stands for in the import, the empty
 * string for an export without a `*`, or `undefined` when they don't match.
 */
function matchExport(
  exportSpecifier: string,
  importSpecifier: string,
): string | undefined {
  const [prefix, suffix] = exportSpecifier.split("*");
  if (prefix === undefined || suffix === undefined) {
    return exportSpecifier === importSpecifier ? "" : undefined;
  }
  const isMatch =
    importSpecifier.length > prefix.length + suffix.length &&
    importSpecifier.startsWith(prefix) &&
    importSpecifier.endsWith(suffix);
  return isMatch
    ? importSpecifier.slice(
        prefix.length,
        importSpecifier.length - suffix.length,
      )
    : undefined;
}
