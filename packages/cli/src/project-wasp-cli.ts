// Hands a Wasp CLI over to the Wasp CLI of the project it runs in. A Wasp CLI
// installed elsewhere, usually globally, runs the project's own `@wasp/cli`
// instead of itself, so a project always runs the Wasp version it installed.

import * as fs from "node:fs/promises";
import { createRequire } from "node:module";
import * as path from "node:path";
import { pathToFileURL } from "node:url";
import { debuglog } from "node:util";
import { CLIError } from "./cli-error.js";
import { doesPathExist } from "./fs-utils.js";

const debug = debuglog("wasp-bin-wrapper");

export interface InstalledWaspCli {
  /** The real path of the CLI's `run.js`, which its package.json exports as `./run`. */
  runModulePath: string;
  version: string;
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
 * installed. It resolves the packages the way Node does from the project dir,
 * through the `exports` of `@wasp/wasp` and `@wasp/cli`. Later Wasp versions
 * must keep those exports, because older global CLIs rely on them.
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
    // This Wasp CLI keeps running and tells the user to run `npm install`.
    debug("No @wasp/wasp installed for the project in %s", waspProjectDir);
    return undefined;
  }

  // With pnpm, `node_modules/@wasp/wasp` is a symlink into pnpm's virtual
  // store, and pnpm links `@wasp/cli` only next to the real package. So we
  // resolve from the real location of `@wasp/wasp`.
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

  const packageJson: { version: string } = JSON.parse(
    await fs.readFile(waspCliPackageJsonPath, "utf8"),
  );
  const waspCli: InstalledWaspCli = {
    runModulePath: await fs.realpath(requireFromWasp.resolve("@wasp/cli/run")),
    version: packageJson.version,
  };
  debug("Found the project's Wasp CLI at %s", waspCli.runModulePath);
  return waspCli;
}

/**
 * Finds the Wasp project dir the way the Wasp CLI does, by searching up from
 * the cwd for a `.wasproot` file.
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
