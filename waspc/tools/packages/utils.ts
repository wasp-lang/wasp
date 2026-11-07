import { join } from "node:path";
import { getPackageJson, getRepoRootPath } from "../utils.ts";

const WASP_NPM_SCOPE = "@wasp/";

// The dir of `@wasp/wasp`, the package Wasp projects depend on.
export const WASP_MAIN_PACKAGE_DIR_NAME = "wasp";

const waspMainPackageJson = getPackageJson(
  getPackageDirPath(WASP_MAIN_PACKAGE_DIR_NAME),
);

// The dirs of the packages `@wasp/wasp` always brings, e.g. `cli` and `spec`.
export const REQUIRED_WASP_NPM_SUBPACKAGES_DIR_NAMES: string[] =
  getWaspPackageDirNames(Object.keys(waspMainPackageJson.dependencies ?? {}));

// The dirs of the packages a project can add next to `@wasp/wasp`, which lists
// them as optional peer dependencies. None yet.
export const OPTIONAL_WASP_NPM_SUBPACKAGES_DIR_NAMES: string[] =
  getWaspPackageDirNames(
    Object.keys(waspMainPackageJson.peerDependencies ?? {}).filter(
      (packageName) =>
        waspMainPackageJson.peerDependenciesMeta?.[packageName]?.optional,
    ),
  );

export const WASP_NPM_SUBPACKAGES_DIR_NAMES: string[] = [
  ...REQUIRED_WASP_NPM_SUBPACKAGES_DIR_NAMES,
  ...OPTIONAL_WASP_NPM_SUBPACKAGES_DIR_NAMES,
];

// The dirs of the packages we publish to npm at the Wasp version: `@wasp/wasp`
// and its subpackages.
export const WASP_PACKAGE_DIR_NAMES: string[] = [
  WASP_MAIN_PACKAGE_DIR_NAME,
  ...WASP_NPM_SUBPACKAGES_DIR_NAMES,
];

export function getPackageDirPath(packageDirName: string): string {
  return join(getRepoRootPath(), "packages", packageDirName);
}

export function getPackageName(packageDirName: string): string {
  return `${WASP_NPM_SCOPE}${packageDirName}`;
}

function getWaspPackageDirNames(packageNames: string[]): string[] {
  return packageNames
    .filter((packageName) => packageName.startsWith(WASP_NPM_SCOPE))
    .map((packageName) => packageName.slice(WASP_NPM_SCOPE.length));
}
