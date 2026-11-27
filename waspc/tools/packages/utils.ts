import { join } from "node:path";
import { getPackageJson, getRepoRootPath, runCmd } from "../utils.ts";

const WASP_NPM_SCOPE = "@wasp/";

export type WaspPackageName = `${typeof WASP_NPM_SCOPE}${string}`;

// `@wasp/wasp`, the package Wasp projects depend on.
export const WASP_MAIN_PACKAGE_NAME: WaspPackageName = `${WASP_NPM_SCOPE}wasp`;

const waspMainPackageJson = getPackageJson(
  getPackageDirPath(WASP_MAIN_PACKAGE_NAME),
);

// The packages `@wasp/wasp` always brings, e.g. `@wasp/cli` and `@wasp/spec`.
const REQUIRED_WASP_NPM_SUBPACKAGE_NAMES: WaspPackageName[] = Object.keys(
  waspMainPackageJson.dependencies ?? {},
).filter(isWaspPackageName);

// The packages a project can add next to `@wasp/wasp`, which lists them as
// optional peer dependencies, e.g. `@wasp/test-utils`.
const OPTIONAL_WASP_NPM_SUBPACKAGE_NAMES: WaspPackageName[] = Object.keys(
  waspMainPackageJson.peerDependencies ?? {},
)
  .filter(isWaspPackageName)
  .filter(
    (packageName) =>
      waspMainPackageJson.peerDependenciesMeta?.[packageName]?.optional,
  );

export const WASP_NPM_SUBPACKAGE_NAMES: WaspPackageName[] = [
  ...REQUIRED_WASP_NPM_SUBPACKAGE_NAMES,
  ...OPTIONAL_WASP_NPM_SUBPACKAGE_NAMES,
];

// The packages we publish to npm at the Wasp version: `@wasp/wasp` and its
// subpackages.
export const WASP_PACKAGE_NAMES: WaspPackageName[] = [
  WASP_MAIN_PACKAGE_NAME,
  ...WASP_NPM_SUBPACKAGE_NAMES,
];

/** `@wasp/cli` is in `packages/cli`. */
export function getPackageDirPath(packageName: WaspPackageName): string {
  return join(
    getRepoRootPath(),
    "packages",
    packageName.slice(WASP_NPM_SCOPE.length),
  );
}

function isWaspPackageName(
  packageName: string,
): packageName is WaspPackageName {
  return packageName.startsWith(WASP_NPM_SCOPE);
}

/**
 * Builds the packages in dependency order, so each one finds the build output
 * of the packages it depends on.
 */
export function buildWaspPackages(packageNames: WaspPackageName[]): void {
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
