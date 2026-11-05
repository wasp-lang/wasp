// Hands a Wasp CLI over to the Wasp CLI of the project it runs in.

import { existsSync } from "node:fs";
import * as fs from "node:fs/promises";
import { createRequire } from "node:module";
import * as path from "node:path";
import { pathToFileURL } from "node:url";
import { debuglog } from "node:util";
import { CLIError } from "./cli-error.js";

const debug = debuglog("wasp-bin-wrapper");

export interface InstalledWaspCli {
  /** The real path of the CLI's `run.js`, which its package.json exports as `./run`. */
  runModulePath: string;
  version: string;
}

/** Returns this Wasp CLI, the one whose code is running. */
export async function getThisWaspCli(): Promise<InstalledWaspCli> {
  return readInstalledWaspCli(
    path.join(import.meta.dirname, "../package.json"),
    path.join(import.meta.dirname, "run.js"),
  );
}

/**
 * Returns the project's Wasp CLI that `thisWaspCli` should hand over to, or
 * `undefined` if `thisWaspCli` should run itself. That's when:
 *
 * - the cwd isn't in a Wasp project,
 * - the project has no `@wasp/wasp` installed, or
 * - `thisWaspCli` is the project's Wasp CLI.
 */
export async function findProjectWaspCliToHandOverTo(
  thisWaspCli: InstalledWaspCli,
): Promise<InstalledWaspCli | undefined> {
  const projectWaspCli = await findProjectWaspCli();
  if (projectWaspCli?.runModulePath === thisWaspCli.runModulePath) {
    debug("Running as the project's own Wasp CLI.");
    return undefined;
  }
  return projectWaspCli;
}

/**
 * Runs the project's Wasp CLI in this process, through the `runWaspCli` it
 * exports (see `run.ts`), so there is no second process to keep signals and
 * exit codes in sync with.
 */
export async function runProjectWaspCli(
  projectWaspCli: InstalledWaspCli,
  thisWaspCli: InstalledWaspCli,
): Promise<number> {
  if (projectWaspCli.version !== thisWaspCli.version) {
    console.error(
      `Running Wasp ${projectWaspCli.version} from this project. The Wasp CLI you called is ${thisWaspCli.version}.`,
    );
  }

  debug(
    "Handing over to the project's Wasp CLI: %s",
    projectWaspCli.runModulePath,
  );
  const { runWaspCli }: typeof import("./run.js") = await import(
    pathToFileURL(projectWaspCli.runModulePath).href
  );
  return runWaspCli();
}

/**
 * Returns the Wasp CLI that the project's `@wasp/wasp` pins, or `undefined` if
 * the cwd isn't in a Wasp project or the project has no `@wasp/wasp`
 * installed. It finds them through Node's module resolution.
 */
async function findProjectWaspCli(): Promise<InstalledWaspCli | undefined> {
  const waspProjectDir = findWaspProjectDir();
  if (waspProjectDir === undefined) {
    return undefined;
  }

  const requireFromProject = createRequire(
    path.join(waspProjectDir, "package.json"),
  );
  const waspPackageJsonPath = tryResolving(
    requireFromProject,
    "@wasp/wasp/package.json",
  );
  if (waspPackageJsonPath === undefined) {
    debug("No @wasp/wasp installed for the project in %s", waspProjectDir);
    return undefined;
  }

  // Resolve `@wasp/cli` from where `@wasp/wasp` really is. With pnpm,
  // `node_modules/@wasp/wasp` is a symlink, and `@wasp/cli` is only next to
  // the symlink's target.
  const requireFromWasp = createRequire(await fs.realpath(waspPackageJsonPath));
  const waspCliPackageJsonPath = tryResolving(
    requireFromWasp,
    "@wasp/cli/package.json",
  );
  if (waspCliPackageJsonPath === undefined) {
    throw new CLIError(
      `The project's \`@wasp/wasp\` in ${path.dirname(waspPackageJsonPath)} is missing its \`@wasp/cli\` dependency. Run \`npm install\` in the project.`,
    );
  }

  const waspCli = await readInstalledWaspCli(
    waspCliPackageJsonPath,
    requireFromWasp.resolve("@wasp/cli/run"),
  );
  debug("Found the project's Wasp CLI at %s", waspCli.runModulePath);
  return waspCli;
}

async function readInstalledWaspCli(
  waspCliPackageJsonPath: string,
  runModulePath: string,
): Promise<InstalledWaspCli> {
  const waspCliPackageJson: { version: string } = JSON.parse(
    await fs.readFile(waspCliPackageJsonPath, "utf8"),
  );
  return {
    runModulePath: await fs.realpath(runModulePath),
    version: waspCliPackageJson.version,
  };
}

/**
 * Searches up from the cwd for a `.wasproot` file. Keep in sync with
 * `findWaspProjectRoot` in
 * https://github.com/wasp-lang/wasp/blob/main/waspc/cli/src/Wasp/Cli/Command/Require/InWaspProject.hs
 */
function findWaspProjectDir(): string | undefined {
  let dir = process.cwd();
  while (true) {
    if (existsSync(path.join(dir, ".wasproot"))) {
      return dir;
    }
    const parentDir = path.dirname(dir);
    if (parentDir === dir) {
      return undefined;
    }
    dir = parentDir;
  }
}

/** Resolves `request`, or returns `undefined` if no installed package has it. */
function tryResolving(
  require: NodeJS.Require,
  request: string,
): string | undefined {
  try {
    return require.resolve(request);
  } catch (error: unknown) {
    if ((error as { code?: string }).code === "MODULE_NOT_FOUND") {
      return undefined;
    }
    throw error;
  }
}
