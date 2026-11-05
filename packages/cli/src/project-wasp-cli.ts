// Hands a Wasp CLI over to the Wasp CLI of the project it runs in.

import * as fs from "node:fs/promises";
import { createRequire } from "node:module";
import * as path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { debuglog } from "node:util";
import { CLIError } from "./cli-error.js";
import { doesPathExist } from "./fs-utils.js";

const debug = debuglog("wasp-bin-wrapper");

export interface InstalledWaspCli {
  /** The real path of the CLI's `run.js`, which its package.json exports as `./run`. */
  runModulePath: string;
  version: string;
}

/** Returns this Wasp CLI, the one whose code is running. */
export async function getThisWaspCli(): Promise<InstalledWaspCli> {
  return readInstalledWaspCli(
    fileURLToPath(new URL("../package.json", import.meta.url)),
    fileURLToPath(import.meta.resolve("./run.js")),
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
 *
 * Warning: changing the `exports` of `@wasp/wasp` or `@wasp/cli` breaks the
 * hand-over from older Wasp CLIs.
 */
async function findProjectWaspCli(): Promise<InstalledWaspCli | undefined> {
  const waspProjectDir = await findWaspProjectDir();
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
    // Not an error: commands like `wasp clean` don't need `@wasp/wasp`. This
    // Wasp CLI runs itself, and the commands that need `@wasp/wasp` explain
    // how to install it.
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
async function findWaspProjectDir(): Promise<string | undefined> {
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
