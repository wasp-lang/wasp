// Finds the Wasp CLI of the project that a Wasp CLI runs in.

import { existsSync } from "node:fs";
import * as fs from "node:fs/promises";
import { createRequire } from "node:module";
import * as path from "node:path";
import { CLIError } from "./cli-error.js";
import { debug } from "./debug.js";
import type { InstalledWaspCli } from "./installed-wasp-cli.js";

const WASP_PACKAGE_NAME = "@wasp/wasp";
const WASP_CLI_PACKAGE_NAME = "@wasp/cli";

/**
 * Finds the CLI the project's `@wasp/wasp` pins with Node's resolution
 * algorithm. `undefined` when the dir is outside of a project, and in a
 * project without an installed `@wasp/wasp`.
 *
 * @param startDir A dir inside the project, e.g. the cwd.
 */
export async function findProjectWaspCli(
  startDir: string,
): Promise<InstalledWaspCli | undefined> {
  const waspProjectDir = findWaspProjectDir(startDir);
  if (waspProjectDir === undefined) {
    return undefined;
  }

  const waspPackageJsonPath = resolveWaspPackageJsonPath(waspProjectDir);
  if (waspPackageJsonPath === undefined) {
    debug(
      "No %s installed for the project in %s",
      WASP_PACKAGE_NAME,
      waspProjectDir,
    );
    return undefined;
  }

  const waspCli = await readWaspCliOf(waspPackageJsonPath);
  debug("Found the project's Wasp CLI at %s", waspCli.runModulePath);
  return waspCli;
}

/**
 * Searches up from the dir for a `.wasproot` file, which marks the root dir of
 * a Wasp project. Keep in sync with `findWaspProjectRoot` in
 * `waspc/cli/src/Wasp/Cli/Command/Require/InWaspProject.hs`.
 */
function findWaspProjectDir(startDir: string): string | undefined {
  let dir = startDir;
  while (true) {
    if (existsSync(path.join(dir, ".wasproot"))) {
      return dir;
    }
    const parentDir = path.dirname(dir);
    const isRootDir = parentDir === dir;
    if (isRootDir) {
      return undefined;
    }
    dir = parentDir;
  }
}

/**
 * Resolves from the project dir, like `evaluateOnInstalledWaspNpmPackageJson`
 * in `waspc/src/Wasp/Project/WaspNpmPackage.hs`.
 */
function resolveWaspPackageJsonPath(
  waspProjectDir: string,
): string | undefined {
  // Without the trailing separator, Node resolves from the parent dir.
  const requireFromProject = createRequire(waspProjectDir + path.sep);
  return tryResolving(requireFromProject, `${WASP_PACKAGE_NAME}/package.json`);
}

/** Reads the `@wasp/cli` that the `@wasp/wasp` with this `package.json` depends on. */
async function readWaspCliOf(
  waspPackageJsonPath: string,
): Promise<InstalledWaspCli> {
  // Resolve `@wasp/cli` from where `@wasp/wasp` really is. With pnpm,
  // `node_modules/@wasp/wasp` is a symlink, and `@wasp/cli` is only next to
  // the symlink's target.
  const requireFromWasp = createRequire(await fs.realpath(waspPackageJsonPath));

  const waspCliPackageJsonPath = tryResolving(
    requireFromWasp,
    `${WASP_CLI_PACKAGE_NAME}/package.json`,
  );
  if (waspCliPackageJsonPath === undefined) {
    throw new CLIError(
      `Missing \`${WASP_CLI_PACKAGE_NAME}\`. Run \`npm install\` in the project.`,
    );
  }

  const { version }: { version: string } = requireFromWasp(
    waspCliPackageJsonPath,
  );
  return {
    runModulePath: await fs.realpath(
      // See the note in `package.json` about this export.
      requireFromWasp.resolve(`${WASP_CLI_PACKAGE_NAME}/internal/run-wasp-cli`),
    ),
    version,
  };
}

function tryResolving(
  require: NodeJS.Require,
  request: string,
): string | undefined {
  try {
    return require.resolve(request);
  } catch (error: unknown) {
    if (isModuleNotFoundError(error)) {
      return undefined;
    }
    throw error;
  }
}

function isModuleNotFoundError(error: unknown): boolean {
  return (
    error instanceof Error &&
    "code" in error &&
    error.code === "MODULE_NOT_FOUND"
  );
}
