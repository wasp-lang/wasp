// @ts-check

// Hands a Wasp CLI over to the Wasp CLI of the project it runs in. A Wasp CLI
// installed elsewhere, usually globally, runs the project's own `@wasp/cli`
// instead of itself, so a project always runs the Wasp version it installed.
//
// This file ships next to `bin.js` in `@wasp/cli`.

import * as fs from "node:fs";
import { createRequire } from "node:module";
import * as path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { debuglog } from "node:util";
import { CLIError } from "./CLIError.js";

const debug = debuglog("wasp-bin-wrapper");

/**
 * @typedef {object} InstalledWaspCli
 * @property {string} binPath The real path of the CLI's `bin.js`.
 * @property {string} version
 */

/**
 * Returns the Wasp CLI that the project's `@wasp/wasp` pins, or `undefined` if
 * the cwd isn't in a Wasp project or the project has no `@wasp/wasp`
 * installed. A Wasp CLI installed elsewhere, usually globally, hands over to
 * this one, so a project always runs the Wasp version it has installed.
 *
 * @returns {InstalledWaspCli | undefined}
 */
export function findProjectWaspCli() {
  const waspProjectDir = findWaspProjectDir();
  if (waspProjectDir === undefined) {
    return undefined;
  }

  const waspPackageJsonPath = findInstalledPackageJsonPath(
    "@wasp/wasp",
    path.join(waspProjectDir, "package.json"),
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
    fs.realpathSync(waspPackageJsonPath),
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
 * Finds the package.json of the package that a file at `fromFilePath` would
 * import as `packageName`. It searches every `node_modules` dir Node would
 * search, so a monorepo that hoists the package to a parent dir works too.
 *
 * A global Wasp CLI must work with the packages of every later Wasp version.
 * So we don't use `require.resolve`, which goes through a package's
 * `exports`, and a later version may change its `exports`. Only the location
 * of package.json and its `bin` field stay the same, because every package
 * manager relies on them.
 *
 * @param {string} packageName
 * @param {string} fromFilePath
 * @returns {string | undefined}
 */
function findInstalledPackageJsonPath(packageName, fromFilePath) {
  const nodeModulesDirs =
    createRequire(fromFilePath).resolve.paths(packageName) ?? [];
  return nodeModulesDirs
    .map((nodeModulesDir) =>
      path.join(nodeModulesDir, packageName, "package.json"),
    )
    .find((packageJsonPath) => fs.existsSync(packageJsonPath));
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

export function isThisWaspCli(/** @type {InstalledWaspCli} */ waspCli) {
  const thisWaspCliBinPath = fs.realpathSync(
    fileURLToPath(new URL("./bin.js", import.meta.url)),
  );
  const isThis = waspCli.binPath === thisWaspCliBinPath;
  if (isThis) {
    debug("Running as the project's own Wasp CLI.");
  }
  return isThis;
}

/**
 * Runs the project's Wasp CLI in this process, so there is no second process
 * to keep signals and exit codes in sync with.
 */
export async function runProjectWaspCli(
  /** @type {InstalledWaspCli} */ waspCli,
) {
  const thisWaspVersion = await getPackageVersion(
    new URL("./package.json", import.meta.url),
  );
  if (waspCli.version !== thisWaspVersion) {
    console.error(
      `Running Wasp ${waspCli.version} from this project. The Wasp CLI you called is ${thisWaspVersion}.`,
    );
  }

  debug("Handing over to the project's Wasp CLI: %s", waspCli.binPath);
  await import(pathToFileURL(waspCli.binPath).href);
  return process.exitCode ?? 0;
}

async function getPackageVersion(/** @type {URL} */ packageJsonUrl) {
  const { default: pkg } = await import(packageJsonUrl.href, {
    with: { type: "json" },
  });
  return /** @type {string} */ (pkg.version);
}
