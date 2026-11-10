// Hands a Wasp CLI over to the Wasp CLI of the project it runs in.

import { existsSync } from "node:fs";
import * as fs from "node:fs/promises";
import { createRequire } from "node:module";
import * as path from "node:path";
import { pathToFileURL } from "node:url";
import { CLIError } from "./cli-error.js";
import { debug } from "./debug.js";
import { readJsonFromPackageRoot } from "./package-root.js";

export interface InstalledWaspCli {
  /** A real path, so symlinked installs of the same CLI compare equal. */
  runModulePath: string;
  version: string;
}

/**
 * What every version of `@wasp/cli` exports from `./run`. Other versions hand
 * over through it, so it can only grow (see the note in `package.json`).
 */
interface WaspCliRunModule {
  runWaspCli(): Promise<number>;
}

export async function getThisWaspCli(): Promise<InstalledWaspCli> {
  const { version } = await readJsonFromPackageRoot<{ version: string }>(
    "package.json",
  );
  return {
    runModulePath: await fs.realpath(path.join(import.meta.dirname, "run.js")),
    version,
  };
}

/**
 * `undefined` when there's nothing to hand over to: outside a project, without
 * an installed `@wasp/wasp`, or when this is the project's CLI.
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
 * Runs it in this process, so there's no second process to keep signals and
 * exit codes in sync with.
 */
export async function runProjectWaspCli(
  projectWaspCli: InstalledWaspCli,
  thisWaspCli: InstalledWaspCli,
): Promise<number> {
  if (
    projectWaspCli.version !== thisWaspCli.version &&
    !isBashCompletionRequest()
  ) {
    console.error(
      `Running Wasp ${projectWaspCli.version} from this project. The Wasp CLI you called is ${thisWaspCli.version}.`,
    );
  }

  debug(
    "Handing over to the project's Wasp CLI: %s",
    projectWaspCli.runModulePath,
  );
  const { runWaspCli }: WaspCliRunModule = await import(
    pathToFileURL(projectWaspCli.runModulePath).href
  );
  return runWaspCli();
}

/**
 * Bash runs `wasp completion:list` on every TAB and reads only its stdout, so
 * anything that run prints to stderr lands in the user's prompt.
 */
function isBashCompletionRequest(): boolean {
  return process.argv[2] === "completion:list";
}

/** Finds the CLI the project's `@wasp/wasp` pins with Node's resolution algorithm. */
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
 * `findWaspProjectRoot` in `waspc/cli/src/Wasp/Cli/Command/Require/InWaspProject.hs`.
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

/** `undefined` if no installed package has `request`. */
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
