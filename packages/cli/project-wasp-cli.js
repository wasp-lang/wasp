// @ts-check

// Hands a Wasp CLI over to the Wasp CLI of the project it runs in. A Wasp CLI
// installed elsewhere, usually globally, runs the project's own `@wasp/cli`
// instead of itself, so a project always runs the Wasp version it installed.

import * as fs from "node:fs/promises";
import * as path from "node:path";
import { pathToFileURL } from "node:url";
import { debuglog } from "node:util";
import { CLIError } from "./CLIError.js";
import { doesPathExist } from "./fs-utils.js";
import { findInstalledPackageJsonPath } from "./node-resolution.js";

const debug = debuglog("wasp-bin-wrapper");

/**
 * @typedef {object} InstalledWaspCli
 * @property {string} binPath The real path of the CLI's `bin.js`.
 * @property {string} version
 */

/**
 * Returns the project's Wasp CLI that `thisWaspCli` should hand over to, or
 * `undefined` if `thisWaspCli` should run itself. That's when:
 *
 * - the cwd isn't in a Wasp project,
 * - the project has no `@wasp/wasp` installed, or
 * - `thisWaspCli` is the project's Wasp CLI.
 *
 * @param {InstalledWaspCli} thisWaspCli
 * @returns {Promise<InstalledWaspCli | undefined>}
 */
export async function findProjectWaspCliToHandOverTo(thisWaspCli) {
  const projectWaspCli = await findProjectWaspCli();
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
 * @returns {Promise<InstalledWaspCli | undefined>}
 */
async function findProjectWaspCli() {
  const waspProjectDir = await findWaspProjectDir();
  if (waspProjectDir === undefined) {
    return undefined;
  }

  const waspPackageJsonPath = await findInstalledPackageJsonPath(
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
  const waspCliPackageJsonPath = await findInstalledPackageJsonPath(
    "@wasp/cli",
    path.dirname(await fs.realpath(waspPackageJsonPath)),
  );
  if (waspCliPackageJsonPath === undefined) {
    throw new CLIError(
      `The project's \`@wasp/wasp\` in ${path.dirname(waspPackageJsonPath)} is missing its \`@wasp/cli\` dependency. Run \`npm install\` in the project.`,
    );
  }

  const waspCli = await readInstalledWaspCli(waspCliPackageJsonPath);
  debug("Found the project's Wasp CLI at %s", waspCli.binPath);
  return waspCli;
}

/**
 * Finds the Wasp project dir the way the Wasp CLI does, by searching up from
 * the cwd for a `.wasproot` file.
 *
 * @returns {Promise<string | undefined>}
 */
async function findWaspProjectDir() {
  let dir = process.cwd();
  while (true) {
    if (await doesPathExist(path.join(dir, ".wasproot"))) {
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
 * Reads where the `@wasp/cli` package at `waspCliPackageJsonPath` has its
 * `wasp` executable, from the package.json `bin` field package managers use
 * to link it.
 *
 * @param {string} waspCliPackageJsonPath
 * @returns {Promise<InstalledWaspCli>}
 */
async function readInstalledWaspCli(waspCliPackageJsonPath) {
  /** @type {{ version: string, bin: string | Record<string, string> }} */
  const packageJson = JSON.parse(
    await fs.readFile(waspCliPackageJsonPath, "utf8"),
  );
  const binFileName =
    typeof packageJson.bin === "string"
      ? packageJson.bin
      : packageJson.bin.wasp;
  return {
    binPath: await fs.realpath(
      path.join(path.dirname(waspCliPackageJsonPath), binFileName),
    ),
    version: packageJson.version,
  };
}
