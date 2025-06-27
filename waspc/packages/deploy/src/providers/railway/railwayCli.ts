import semver from "semver";
import { $ } from "zx";

import { confirm } from "@inquirer/prompts";
import {
  RailwayCliExe,
  RailwayProjectName,
  SemverVersion,
} from "./brandedTypes.js";

// Railway CLI version 4.0.1 includes a change that is needed for
// Wasp deploy command to work with Railway properly:
// https://github.com/railwayapp/cli/pull/596
const minSupportedRailwayCliVersion = "4.0.1" as SemverVersion;

export async function ensureRailwayCliReady(
  railwayExe: RailwayCliExe,
): Promise<void> {
  const railwayCliVersion = await getRailwayCliVersion(railwayExe);
  if (railwayCliVersion === null) {
    const message = [
      "The Railway CLI is not available on this system.",
      "Read how to install the Railway CLI here: https://docs.railway.com/guides/cli",
    ].join("\n");
    throw new Error(message);
  }

  if (!isUsingMinimumSupportedRailwayCliVersion(railwayCliVersion!)) {
    const message = [
      `Wasp expects at least Railway CLI version ${minSupportedRailwayCliVersion}.`,
      "Read how to update the Railway CLI here: https://docs.railway.com/guides/cli",
    ].join("\n");
    throw new Error(message);
  }
  await ensureUserLoggedIn(railwayExe);
}

async function ensureUserLoggedIn(railwayExe: RailwayCliExe): Promise<void> {
  const userLoggedIn = await isUserLoggedIn(railwayExe);
  if (userLoggedIn) {
    return;
  }

  await confirmWithUserTheyWantToLogin();
  await loginToRailway(railwayExe);
}

async function isUserLoggedIn(railwayExe: RailwayCliExe): Promise<boolean> {
  const result = await $({
    nothrow: true,
  })`${railwayExe} whoami`;
  return result.exitCode === 0;
}

async function confirmWithUserTheyWantToLogin(): Promise<void> {
  const wantsToLogin = await confirm({
    message: "You are not logged into Railway. Would you like to log in now?",
  });
  if (!wantsToLogin) {
    throw new Error("Unable to continue without logging in to Railway.");
  }
}

async function loginToRailway(railwayExe: RailwayCliExe): Promise<void> {
  // Login comand requires **interactive** terminal
  const loginCmdOptions = {
    stdio: "inherit",
  } as const;
  await $(loginCmdOptions)`${railwayExe} login`;
}

async function getRailwayCliVersion(
  railwayExe: RailwayCliExe,
): Promise<SemverVersion | null> {
  const result = await $`${railwayExe} -V`;
  const match = result.stdout.match(/railway(?:app)? (\d+\.\d+\.\d+)/);
  if (match !== null) {
    return match[1] as SemverVersion;
  } else {
    return null;
  }
}

function isUsingMinimumSupportedRailwayCliVersion(
  railwayCliVersion: SemverVersion,
): boolean {
  return semver.gte(railwayCliVersion, minSupportedRailwayCliVersion);
}

export function assertRailwayProjectNameIsValid(
  projectName: RailwayProjectName,
): void {
  const maximumProjectNameLength = getMaximumProjectNameLength();

  if (projectName.length > maximumProjectNameLength) {
    throw new Error(
      `The project name "${projectName}" is too long (${projectName.length} characters). It must be at most ${maximumProjectNameLength} characters long.`,
    );
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
