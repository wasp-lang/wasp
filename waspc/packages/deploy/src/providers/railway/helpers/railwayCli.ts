import { exit } from "process";
import semver from "semver";
import { $ } from "zx";

import { confirm } from "@inquirer/prompts";
import { Branded } from "../../../common/branded.js";
import { waspSays } from "../../../common/output.js";
import { RailwayCliExe } from "../CommonOptions.js";
import { RailwayProjectName } from "../DeploymentInfo.js";

// Railway CLI version 4.0.1 includes a change that is needed for
// Wasp deploy command to work with Railway properly:
// https://github.com/railwayapp/cli/pull/596
const minSupportedRailwayCliVersion = "4.0.1";

type RailwayCliVersion = Branded<string, "RailwayCliVersion">;

export async function ensureRailwayReady(
  railwayExe: RailwayCliExe,
): Promise<void> {
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

async function ensureUserLoggedIn(railwayExe: RailwayCliExe): Promise<void> {
  const userLoggedIn = await isUserLoggedIn(railwayExe);
  if (userLoggedIn) {
    return;
  }

  const wantsToLogin = await confirm({
    message:
      "railway is not logged into Railway. Would you like to log in now?",
  });
  if (!wantsToLogin) {
    waspSays("Ok, exiting.");
    exit(1);
  }

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

async function isUserLoggedIn(railwayExe: RailwayCliExe): Promise<boolean> {
  try {
    await $`${railwayExe} whoami`;
    return true;
  } catch {
    return false;
  }
}

async function getRailwayCliVersion(
  railwayExe: RailwayCliExe,
): Promise<RailwayCliVersion | null> {
  try {
    const result = await $`${railwayExe} -V`;
    const match = result.stdout.match(/railway(?:app)? (\d+\.\d+\.\d+)/);
    if (match !== null) {
      return match[1] as RailwayCliVersion;
    } else {
      return null;
    }
  } catch {
    return null;
  }
}

function isUsingMinimumSupportedRailwayCliVersion(
  railwayCliVersion: RailwayCliVersion,
): boolean {
  return semver.gte(railwayCliVersion, minSupportedRailwayCliVersion);
}

export async function assertRailwayProjectNameIsValid(
  projectName: RailwayProjectName,
): Promise<void> {
  const maximumProjectNameLength = getMaximumProjectNameLength();

  if (projectName.length > maximumProjectNameLength) {
    waspSays(
      `The project name "${projectName}" is too long (${projectName.length} characters). It must be at most ${maximumProjectNameLength} characters long.`,
    );
    exit(1);
  }
}

function getMaximumProjectNameLength(): number {
  // Railway has a limit of 32 characters for the service name.
  // https://docs.railway.com/reference/services#constraints
  const maximumServiceNameLength = 32;
  // We construct service names by appending suffixes to the project name.
  const suffixes = ["-server", "-client", "-db"];
  const maximumSuffixLength = Math.max(...suffixes.map((s) => s.length));
  return maximumServiceNameLength - maximumSuffixLength;
}
