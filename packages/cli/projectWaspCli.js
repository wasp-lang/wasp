// @ts-check

// Hands a Wasp CLI over to the Wasp CLI of the project it runs in. A Wasp CLI
// installed elsewhere, usually globally, runs the project's own `@wasp/cli`
// instead of itself, so a project always runs the Wasp version it installed.
//
// This file ships next to `bin.js` in `@wasp/cli`.

import * as fs from "node:fs";
import * as path from "node:path";
import { pathToFileURL } from "node:url";
import { debuglog } from "node:util";
import { CLIError } from "./CLIError.js";

const debug = debuglog("wasp-bin-wrapper");

/**
 * @typedef {object} InstalledWaspCli
 * @property {string} binPath The real path of the CLI's `bin.js`.
 * @property {string} version
 */

/**
 * Returns the project's Wasp CLI that `thisWaspCli` should hand over to, or
 * `undefined` if `thisWaspCli` should run itself. That's when the cwd isn't in
 * a Wasp project, the project has no `@wasp/wasp` installed, or `thisWaspCli`
 * is the project's Wasp CLI.
 *
 * @param {InstalledWaspCli} thisWaspCli
 * @returns {InstalledWaspCli | undefined}
 */
export function findProjectWaspCliToHandOverTo(thisWaspCli) {
  const projectWaspCli = findProjectWaspCli();
  if (projectWaspCli?.binPath === thisWaspCli.binPath) {
    debug("Running as the project's own Wasp CLI.");
    return undefined;
  }
  return projectWaspCli;
}

/**
 * Runs the project's Wasp CLI in this process, so there is no second process
 * to keep signals and exit codes in sync with. Importing its `bin.js` runs it,
 * and it reports its result through `process.exitCode`. Every `bin.js` keeps
 * that contract, see the comment at its top.
 *
 * @param {InstalledWaspCli} projectWaspCli
 * @param {InstalledWaspCli} thisWaspCli
 */
export async function runProjectWaspCli(projectWaspCli, thisWaspCli) {
  if (projectWaspCli.version !== thisWaspCli.version) {
    console.error(
      `Running Wasp ${projectWaspCli.version} from this project. The Wasp CLI you called is ${thisWaspCli.version}.`,
    );
  }

  debug("Handing over to the project's Wasp CLI: %s", projectWaspCli.binPath);
  await import(pathToFileURL(projectWaspCli.binPath).href);
  return process.exitCode ?? 0;
}

/**
 * Returns the Wasp CLI that the project's `@wasp/wasp` pins, or `undefined` if
 * the cwd isn't in a Wasp project or the project has no `@wasp/wasp`
 * installed.
 *
 * @returns {InstalledWaspCli | undefined}
 */
function findProjectWaspCli() {
  const waspProjectDir = findWaspProjectDir();
  if (waspProjectDir === undefined) {
    return undefined;
  }

  const waspPackageJsonPath = findInstalledPackageJsonPath(
    "@wasp/wasp",
    waspProjectDir,
  );
  if (waspPackageJsonPath === undefined) {
    // This Wasp CLI keeps running and tells the user to run `npm install`.
    debug("No @wasp/wasp installed for the project in %s", waspProjectDir);
    return undefined;
  }

  // With pnpm, `node_modules/@wasp/wasp` is a symlink into pnpm's virtual
  // store, and pnpm links `@wasp/cli` only next to the real package. So we
  // search from the real location of `@wasp/wasp`.
  const waspCliPackageJsonPath = findInstalledPackageJsonPath(
    "@wasp/cli",
    path.dirname(fs.realpathSync(waspPackageJsonPath)),
  );
  if (waspCliPackageJsonPath === undefined) {
    throw new CLIError(
      `The project's \`@wasp/wasp\` in ${path.dirname(waspPackageJsonPath)} is missing its \`@wasp/cli\` dependency. Run \`npm install\` in the project.`,
    );
  }

  const waspCli = readInstalledWaspCli(waspCliPackageJsonPath);
  debug("Found the project's Wasp CLI at %s", waspCli.binPath);
  return waspCli;
}

/**
 * Finds the Wasp project dir the way the Wasp CLI does, by searching up from
 * the cwd for a `.wasproot` file.
 */
function findWaspProjectDir() {
  let dir = process.cwd();
  while (true) {
    if (fs.existsSync(path.join(dir, ".wasproot"))) {
      return dir;
    }
    const parentDir = path.dirname(dir);
    if (parentDir === dir) {
      return undefined;
    }
    dir = parentDir;
  }
}

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
 * @returns {string | undefined}
 */
function findInstalledPackageJsonPath(packageName, fromDir) {
  return getNodeModulesDirs(fromDir)
    .map((nodeModulesDir) =>
      path.join(nodeModulesDir, packageName, "package.json"),
    )
    .find((packageJsonPath) => fs.existsSync(packageJsonPath));
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

/**
 * Reads where the `@wasp/cli` package at `waspCliPackageJsonPath` has its
 * `wasp` executable, from the package.json `bin` field package managers use
 * to link it.
 *
 * @param {string} waspCliPackageJsonPath
 * @returns {InstalledWaspCli}
 */
function readInstalledWaspCli(waspCliPackageJsonPath) {
  /** @type {{ version: string, bin: string | Record<string, string> }} */
  const packageJson = JSON.parse(
    fs.readFileSync(waspCliPackageJsonPath, "utf8"),
  );
  const binFileName =
    typeof packageJson.bin === "string"
      ? packageJson.bin
      : packageJson.bin.wasp;
  return {
    binPath: fs.realpathSync(
      path.join(path.dirname(waspCliPackageJsonPath), binFileName),
    ),
    version: packageJson.version,
  };
}
