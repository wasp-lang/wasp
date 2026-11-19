import { join } from "node:path";
import { getPackageJson, getRepoRootPath, runCmd } from "../utils.ts";

const WASP_NPM_SCOPE = "@wasp/";

// `@wasp/wasp`, the package Wasp projects depend on.
export const WASP_MAIN_PACKAGE_NAME = `${WASP_NPM_SCOPE}wasp`;

const waspMainPackageJson = getPackageJson(
  getPackageDirPath(WASP_MAIN_PACKAGE_NAME),
);

// The packages `@wasp/wasp` always brings, e.g. `@wasp/cli` and `@wasp/spec`.
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

/** `@wasp/cli` is in `packages/cli`. */
export function getPackageDirPath(packageName: string): string {
  return join(
    getRepoRootPath(),
    "packages",
    packageName.slice(WASP_NPM_SCOPE.length),
  );
}

function isWaspPackageName(packageName: string): boolean {
  return packageName.startsWith(WASP_NPM_SCOPE);
}

/**
 * Builds the packages in dependency order, so each one finds the build output
 * of the packages it depends on.
 */
export function buildWaspPackages(packageNames: string[]): void {
  runCmd(
    "pnpm",
    [
      ...packageNames.flatMap((packageName) => ["--filter", packageName]),
      "--fail-if-no-match",
      "run",
      "--if-present",
      "build",
    ],
    { cwd: getRepoRootPath(), stdio: "inherit" },
  );
}
