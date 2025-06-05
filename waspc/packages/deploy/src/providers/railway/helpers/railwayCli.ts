import { Command } from "commander";
import { exit } from "process";
import semver from "semver";
import { $, ProcessOutput, question } from "zx";

import { isYes, waspSays } from "../../../helpers.js";

// Railway CLI version 4.0.1 includes a change that is needed for
// Wasp deploy command to work with Railway properly:
// https://github.com/railwayapp/cli/pull/596
const minSupportedRailwayCliVersion = "4.0.1";

async function ensureUserLoggedIn(railwayExe: string): Promise<void> {
  const userLoggedIn = await isUserLoggedIn(railwayExe);
  if (userLoggedIn) {
    return;
  }

  const answer = await question(
    "railway is not logged into Railway. Would you like to log in now? ",
  );
  if (!isYes(answer)) {
    waspSays("Ok, exiting.");
    exit(1);
  }

  // In the CI, users are expected to set the Railway token
  // as an environment variable.
  // https://docs.railway.com/guides/cli#tokens
  try {
    await $({
      // Login comand requires **interactive** terminal
      stdio: "inherit",
    })`${railwayExe} login`;
  } catch {
    waspSays(
      'It seems there was a problem logging in. Please run "railway login" and try again.',
    );
    exit(1);
  }
}

async function isUserLoggedIn(railwayExe: string): Promise<boolean> {
  try {
    await $`${railwayExe} whoami`;
    return true;
  } catch {
    return false;
  }
}

export async function ensureRailwayReady(thisCommand: Command): Promise<void> {
  const railwayExe = thisCommand.opts().railwayExe;
  const railwayCliVersion = await getRailwayCliVersion(railwayExe);
  if (railwayCliVersion === null) {
    waspSays("The Railway CLI is not available on this system.");
    waspSays(
      "Read how to install the Railway CLI here: https://docs.railway.com/guides/cli",
    );
    exit(1);
  }

  if (!isUsingMinimumSupportedRailwayCliVersion(railwayCliVersion!)) {
    waspSays(
      `Wasp expects at least Railway CLI version ${minSupportedRailwayCliVersion}.`,
    );
    waspSays(
      "Read how to update the Railway CLI here: https://docs.railway.com/guides/cli",
    );
    exit(1);
  }
  await ensureUserLoggedIn(railwayExe);
}

async function getRailwayCliVersion(
  railwayExe: string,
): Promise<string | null> {
  try {
    const result: ProcessOutput = await $`${railwayExe} -V`;
    const match = result.stdout.match(/railway(?:app)? (\d+\.\d+\.\d+)/);
    if (match !== null) {
      return match[1];
    } else {
      return null;
    }
  } catch {
    return null;
  }
}

function isUsingMinimumSupportedRailwayCliVersion(
  railwayCliVersion: string,
): boolean {
  return semver.gte(railwayCliVersion, minSupportedRailwayCliVersion);
}

export async function ensureRailwayBasenameIsValid(
  thisCommand: Command,
): Promise<void> {
  // Railway has a limit of 32 characters for the service name.
  // https://docs.railway.com/reference/services#constraints
  const maximumServiceNameLength = 32;
  const suffixes = ["-server", "-client", "-db"];
  const maximumSuffixLength = Math.max(...suffixes.map((s) => s.length));
  const maximumBasenameLength = maximumServiceNameLength - maximumSuffixLength;
  const basename = thisCommand.args[0];

  if (basename.length > maximumBasenameLength) {
    waspSays(
      `The basename "${basename}" is too long (${basename.length} characters). It must be at most ${maximumBasenameLength} characters long.`,
    );
    exit(1);
  }
}
