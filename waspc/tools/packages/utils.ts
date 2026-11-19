import { join } from "node:path";
import { getPackageJson, getRepoRootPath } from "../utils.ts";

const WASP_NPM_SCOPE = "@wasp/";
const WASP_LIB_PACKAGE_NAME_PREFIX = `${WASP_NPM_SCOPE}lib-`;

// `@wasp/wasp`, the package Wasp projects depend on.
export const WASP_MAIN_PACKAGE_NAME = `${WASP_NPM_SCOPE}wasp`;

const waspMainPackageJson = getPackageJson(
  getPackageDirPath(WASP_MAIN_PACKAGE_NAME),
);

// The packages `@wasp/wasp` always brings, e.g. `@wasp/cli`, `@wasp/spec` and
// the Wasp libs.
export const REQUIRED_WASP_NPM_SUBPACKAGE_NAMES: string[] = Object.keys(
  waspMainPackageJson.dependencies ?? {},
).filter(isWaspPackageName);

// The packages a project can add next to `@wasp/wasp`, which lists them as
// optional peer dependencies. None yet.
export const OPTIONAL_WASP_NPM_SUBPACKAGE_NAMES: string[] = Object.keys(
  waspMainPackageJson.peerDependencies ?? {},
).filter(
  (packageName) =>
    isWaspPackageName(packageName) &&
    waspMainPackageJson.peerDependenciesMeta?.[packageName]?.optional,
);

export const WASP_NPM_SUBPACKAGE_NAMES: string[] = [
  ...REQUIRED_WASP_NPM_SUBPACKAGE_NAMES,
  ...OPTIONAL_WASP_NPM_SUBPACKAGE_NAMES,
];

// The packages we publish to npm at the Wasp version: `@wasp/wasp` and its
// subpackages.
export const WASP_PACKAGE_NAMES: string[] = [
  WASP_MAIN_PACKAGE_NAME,
  ...WASP_NPM_SUBPACKAGE_NAMES,
];

/** `@wasp/cli` is in `packages/cli`, and `@wasp/lib-auth` in `packages/libs/auth`. */
export function getPackageDirPath(packageName: string): string {
  const packagesDirPath = join(getRepoRootPath(), "packages");
  return isWaspLibPackageName(packageName)
    ? join(
        packagesDirPath,
        "libs",
        packageName.slice(WASP_LIB_PACKAGE_NAME_PREFIX.length),
      )
    : join(packagesDirPath, packageName.slice(WASP_NPM_SCOPE.length));
}

export function isWaspLibPackageName(packageName: string): boolean {
  return packageName.startsWith(WASP_LIB_PACKAGE_NAME_PREFIX);
}

function isWaspPackageName(packageName: string): boolean {
  return packageName.startsWith(WASP_NPM_SCOPE);
}
