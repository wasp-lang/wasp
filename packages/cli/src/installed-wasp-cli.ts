import thisPackageJson from "#/package.json" with { type: "json" };
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
      fileURLToPath(import.meta.resolve("#/run-wasp-cli")),
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
type WaspCliRunModule = typeof import("./run-wasp-cli.js");

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
