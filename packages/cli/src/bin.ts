#!/usr/bin/env node

import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { debuglog } from "node:util";
import { CLIError } from "./cli-error.js";
import { doesPathExist } from "./fs-utils.js";
import {
  findProjectWaspCliToHandOverTo,
  type InstalledWaspCli,
  runProjectWaspCli,
} from "./project-wasp-cli.js";
import { getWaspCliVersion, runWaspCli } from "./run.js";

// If you want to turn on debug logging, set the following environment variable:
// `NODE_DEBUG=wasp-bin-wrapper`
const debug = debuglog("wasp-bin-wrapper");

try {
  const childProcessExitCode = await main();
  debug("Wasp process exited with code: %d", childProcessExitCode);
  process.exitCode = childProcessExitCode;
} catch (error) {
  debug("Error in launching Wasp: %o", error);
  // An error happened while trying to find or run the Wasp executable.
  // Report it and exit with an unknown failure error code.
  // Errors inside the Wasp process itself are not in this wrapper, as they are
  // handled by mimicking its exit code above.
  CLIError.log(error);
  process.exitCode = -1;
} finally {
  debug("Exiting Wasp wrapper process with code: %d", process.exitCode);
}

async function main(): Promise<number> {
  await assertNoLegacyInstallerConflict();

  const thisWaspCli: InstalledWaspCli = {
    runModulePath: await fs.realpath(
      fileURLToPath(import.meta.resolve("./run.js")),
    ),
    version: await getWaspCliVersion(),
  };

  const projectWaspCli = await findProjectWaspCliToHandOverTo(thisWaspCli);
  if (projectWaspCli !== undefined) {
    return runProjectWaspCli(projectWaspCli, thisWaspCli).catch(
      CLIError.rethrowWith("Failed to run the project's Wasp CLI."),
    );
  }

  return runWaspCli();
}

// Wasp used to be installed with a shell installer script into
// `~/.local/share/wasp-lang`. Having both that and the npm package installed
// leads to confusing conflicts, so we refuse to run until the user migrates.
//
// We check on every CLI invocation, not in an install script, because package
// managers increasingly block install scripts of dependencies (pnpm, bun).
//
// TODO: Consider removing this check once we no longer expect any users to
// have a shell-installer based Wasp installation.
async function assertNoLegacyInstallerConflict(): Promise<void> {
  const waspLangDir = tryGetWaspLangDir();
  // No home dir (some containers), so no installer-based Wasp to conflict with.
  if (waspLangDir === undefined) {
    return;
  }

  // The installer script checks for this marker to warn about the same
  // conflict from its side.
  const npmMarkerFile = path.join(waspLangDir, ".uses-npm");
  if (await doesPathExist(npmMarkerFile)) {
    return;
  }

  if (await hasInstallerWasp(waspLangDir)) {
    throw new CLIError(`\
Detected an existing legacy Wasp installation.

You have Wasp installed via the curl installer script. To prevent conflicts,
please migrate to npm before using the npm package.

To migrate, run:

  curl -sSL https://get.wasp.sh/installer.sh | sh -s -- migrate-to-npm

and follow the instructions.`);
  }

  await ensureNpmMarker(waspLangDir, npmMarkerFile);
}

// The path is also defined in:
// - https://github.com/wasp-lang/wasp/blob/main/waspc/cli/src/Wasp/Cli/FileSystem.hs
// - https://github.com/wasp-lang/get-wasp-sh/blob/master/installer.sh
// TODO: Do not hardcode: https://github.com/wasp-lang/wasp/issues/980
function tryGetWaspLangDir(): string | undefined {
  try {
    return path.join(os.homedir(), ".local", "share", "wasp-lang");
  } catch {
    return undefined;
  }
}

async function hasInstallerWasp(waspLangDir: string): Promise<boolean> {
  try {
    // If the folder exists and has any subdirectories (for the installed version(s)),
    // we assume it's an installer-based Wasp installation.
    const entries = await fs.readdir(waspLangDir, { withFileTypes: true });
    return entries.some((entry) => entry.isDirectory());
  } catch {
    return false;
  }
}

async function ensureNpmMarker(
  waspLangDir: string,
  npmMarkerFile: string,
): Promise<void> {
  try {
    await fs.mkdir(waspLangDir, { recursive: true });

    // Same as `touch` - By appending nothing, we create the file if it doesn't
    // exist, or leave the contents unchanged if it does.
    await fs.appendFile(npmMarkerFile, "");
  } catch {
    // Ignore errors - marker file is best-effort
  }
}
