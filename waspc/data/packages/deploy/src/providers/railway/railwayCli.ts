import { SemVer } from "semver";
import { $ } from "zx";

import { confirm } from "@inquirer/prompts";
import {
  assertCliVersionMeetsMinimum,
  parseCliVersion,
} from "../../common/cliVersion.js";
import { RailwayCliExe, RailwayProjectName } from "./brandedTypes.js";
import { serviceNameSuffixes } from "./railwayService/nameGenerator.js";

// Wasp relies on the `railway api` command added in Railway CLI 5.28.0.
const minSupportedRailwayCliVersion = new SemVer("5.28.0");

export async function ensureRailwayCliReady(
  railwayExe: RailwayCliExe,
): Promise<void> {
  const railwayCliVersion = await getRailwayCliVersion(railwayExe);
  assertCliVersionMeetsMinimum({
    cliName: "Railway CLI",
    currentVersion: railwayCliVersion,
    minimumVersion: minSupportedRailwayCliVersion,
    updateInstructions:
      "Read how to update the Railway CLI here: https://docs.railway.com/guides/cli",
  });
  await ensureUserLoggedIn(railwayExe);
}

async function ensureUserLoggedIn(railwayExe: RailwayCliExe): Promise<void> {
  const userLoggedIn = await isUserLoggedIn(railwayExe);
  if (userLoggedIn) {
    return;
  }

  await confirmUserWantsToLogin();
  await loginToRailway(railwayExe);
}

async function isUserLoggedIn(railwayExe: RailwayCliExe): Promise<boolean> {
  const result = await $({ nothrow: true })`${railwayExe} whoami`;
  return result.exitCode === 0;
}

async function confirmUserWantsToLogin(): Promise<void> {
  const wantsToLogin = await confirm({
    message: "You are not logged into Railway. Would you like to log in now?",
  });
  if (!wantsToLogin) {
    throw new Error("Unable to continue without logging in to Railway.");
  }
}

async function loginToRailway(railwayExe: RailwayCliExe): Promise<void> {
  // Login comand requires **interactive** terminal.
  const loginCmdOptions = {
    stdio: "inherit",
  } as const;
  await $(loginCmdOptions)`${railwayExe} login`;
}

async function getRailwayCliVersion(
  railwayExe: RailwayCliExe,
): Promise<SemVer> {
  const result = await $({ nothrow: true })`${railwayExe} -V`;

  if (result.exitCode !== 0) {
    throw new Error(
      [
        "Failed to get Railway CLI version. Most likely the Railway CLI is not installed.",
        "Read how to install the Railway CLI here: https://docs.railway.com/guides/cli",
      ].join("\n"),
    );
  }

  const match = result.stdout.match(/railway(?:app)? (.*)/);

  if (match === null) {
    throw new Error(
      `Failed to get Railway CLI version from output "${result.stdout.trim()}".`,
    );
  }

  return parseCliVersion("Railway CLI", match[1]);
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

  // We construct service names by appending suffixes to the project name:
  // ServiceNameLength = ProjectNameLength + SuffixLength.
  return maximumServiceNameLength - getMaximumServiceSuffixLength();
}

function getMaximumServiceSuffixLength(): number {
  return Math.max(...Object.values(serviceNameSuffixes).map((s) => s.length));
}
