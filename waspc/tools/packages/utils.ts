import { join } from "node:path";
import { getPackageJson, getRepoRootPath } from "../utils.ts";

const WASP_NPM_SCOPE = "@wasp/";

// The dir of `@wasp/wasp`, the package Wasp projects depend on.
export const WASP_NPM_PACKAGE_DIR_NAME = "wasp";

// The dirs of the packages `@wasp/wasp` brings, e.g. `cli` and `spec`. We read
// them from its `dependencies`, so a new package only needs to go there.
export const WASP_NPM_PACKAGE_DEPENDENCY_DIR_NAMES: string[] = Object.keys(
  getPackageJson(getPackageDirPath(WASP_NPM_PACKAGE_DIR_NAME)).dependencies ??
    {},
)
  .filter((packageName) => packageName.startsWith(WASP_NPM_SCOPE))
  .map((packageName) => packageName.slice(WASP_NPM_SCOPE.length));

// The dirs of the packages we publish to npm at the Wasp version: `@wasp/wasp`
// and the packages it brings.
export const WASP_PACKAGE_DIR_NAMES: string[] = [
  WASP_NPM_PACKAGE_DIR_NAME,
  ...WASP_NPM_PACKAGE_DEPENDENCY_DIR_NAMES,
];

export function getPackageDirPath(packageDirName: string): string {
  return join(getRepoRootPath(), "packages", packageDirName);
}

export function getPackageName(packageDirName: string): string {
  return `${WASP_NPM_SCOPE}${packageDirName}`;
}
