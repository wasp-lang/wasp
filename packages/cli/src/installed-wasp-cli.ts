// An installed `@wasp/cli`, how to tell two of them apart, and how one hands
// over to another.

import thisPackageJson from "#package.json" with { type: "json" };
import * as fs from "node:fs/promises";
import { fileURLToPath, pathToFileURL } from "node:url";
import { debug } from "./debug.js";

export interface InstalledWaspCli {
  /** A real path, so symlinked installs of the same CLI compare equal. */
  runModulePath: string;
  version: string;
}

export async function getThisWaspCli(): Promise<InstalledWaspCli> {
  return {
    runModulePath: await fs.realpath(
      fileURLToPath(import.meta.resolve("#run-wasp-cli")),
    ),
    version: thisPackageJson.version,
  };
}

export function isSameWaspCli(
  waspCli: InstalledWaspCli,
  otherWaspCli: InstalledWaspCli,
): boolean {
  return waspCli.runModulePath === otherWaspCli.runModulePath;
}

/**
 * What `@wasp/cli` exports from `./internal/run-wasp-cli`.
 */
type WaspCliRunModule = Pick<typeof import("./run-wasp-cli.js"), "runWaspCli">;

/**
 * Runs the other CLI in this process, so there's no second process to keep
 * signals and exit codes in sync with. Returns its exit code.
 */
export async function handOverTo(waspCli: InstalledWaspCli): Promise<number> {
  debug("Handing over to the Wasp CLI at %s", waspCli.runModulePath);
  // Windows paths like `C:\...` aren't valid import specifiers, so import
  // the file URL.
  const { runWaspCli }: WaspCliRunModule = await import(
    pathToFileURL(waspCli.runModulePath).href
  );
  return runWaspCli();
}

/**
 * Tells the user which Wasp version runs, when it isn't the one they called.
 *
 * @param cliArgs The arguments the user called the CLI with.
 */
export function notifyIfVersionsDiffer(
  projectWaspCli: InstalledWaspCli,
  calledWaspCli: InstalledWaspCli,
  cliArgs: string[],
): void {
  if (
    projectWaspCli.version !== calledWaspCli.version &&
    !isBashCompletionRequest(cliArgs)
  ) {
    console.error(
      `Running Wasp ${projectWaspCli.version} from this project. The Wasp CLI you called is ${calledWaspCli.version}.`,
    );
  }
}

/**
 * Bash runs `wasp completion:list` on every TAB and reads only its stdout, so
 * anything that run prints to stderr lands in the user's prompt. Keep the
 * command in sync with `waspc/cli/exe/Main.hs`.
 */
function isBashCompletionRequest(cliArgs: string[]): boolean {
  return cliArgs[0] === "completion:list";
}
