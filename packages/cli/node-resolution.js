// @ts-check

import * as path from "node:path";
import { doesPathExist } from "./fs-utils.js";

/**
 * Finds the package.json of the package that code in `fromDir` would import
 * as `packageName`. Like Node.js, it searches the `node_modules` dir of
 * `fromDir` and of each parent dir, closest first. That covers every package
 * manager's node_modules layout, including monorepos where the package
 * manager hoists dependencies to a parent dir. The Wasp CLI binary finds the
 * project's `@wasp/wasp` with the same rule (`findWaspNpmPackageDir` in
 * `waspc/src/Wasp/Project/WaspNpmPackage.hs`), so the two never disagree.
 *
 * A global Wasp CLI must work with the packages of every later Wasp version.
 * So we don't use `require.resolve`, which goes through a package's
 * `exports`, and a later version may change its `exports`. Only the location
 * of package.json and its `bin` field stay the same, because every package
 * manager relies on them.
 *
 * @param {string} packageName
 * @param {string} fromDir
 * @returns {Promise<string | undefined>}
 */
export async function findInstalledPackageJsonPath(packageName, fromDir) {
  for (const nodeModulesDir of getNodeModulesDirs(fromDir)) {
    const packageJsonPath = path.join(
      nodeModulesDir,
      packageName,
      "package.json",
    );
    if (await doesPathExist(packageJsonPath)) {
      return packageJsonPath;
    }
  }
  return undefined;
}

/**
 * The `node_modules` dirs Node.js searches from `fromDir`, without its global
 * folders. Like Node, it skips dirs that are themselves named `node_modules`.
 *
 * @param {string} fromDir
 * @returns {string[]}
 */
function getNodeModulesDirs(fromDir) {
  const parentDir = path.dirname(fromDir);
  const nodeModulesDirsAbove =
    parentDir === fromDir ? [] : getNodeModulesDirs(parentDir);
  return path.basename(fromDir) === "node_modules"
    ? nodeModulesDirsAbove
    : [path.join(fromDir, "node_modules"), ...nodeModulesDirsAbove];
}
