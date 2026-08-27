import { confirm } from "@inquirer/prompts";
import { SemVer } from "semver";
import { $ } from "zx";
import {
  assertCliVersionMeetsMinimum,
  parseCliVersion,
} from "../../common/cliVersion.js";
import { getFullCommandName } from "../../common/commander.js";
import { createCommand, runJsonCommand } from "../../common/zx.js";
import { executeFlyCommand } from "./index.js";
import {
  FlyRegionListSchema,
  FlySecretListSchema,
} from "./jsonOutputSchemas.js";

const minSupportedFlyCliVersion = new SemVer("0.4.82");

export async function isUserLoggedIn(): Promise<boolean> {
  try {
    await $`flyctl auth whoami`;
    return true;
  } catch {
    return false;
  }
}

async function ensureUserLoggedIn(): Promise<void> {
  const userLoggedIn = await isUserLoggedIn();
  if (userLoggedIn) {
    return;
  }

  const wantsToLogin = await confirm({
    message: "flyctl is not logged into Fly.io. Would you like to log in now?",
  });
  if (!wantsToLogin) {
    throw new Error("Unable to continue without logging in to Fly.io.");
  }

  try {
    await $`flyctl auth login`;
  } catch {
    throw new Error(
      'It seems there was a problem logging in. Please run "flyctl auth login" and try again.',
    );
  }
}

export async function ensureFlyReady(): Promise<void> {
  const flyCliVersion = await getFlyCliVersion();
  assertCliVersionMeetsMinimum({
    cliName: "Fly CLI",
    currentVersion: flyCliVersion,
    minimumVersion: minSupportedFlyCliVersion,
    updateInstructions:
      "Read how to update the Fly CLI here: https://fly.io/docs/hands-on/install-flyctl",
  });
  await ensureUserLoggedIn();
}

async function getFlyCliVersion(): Promise<SemVer> {
  const result = await $({ nothrow: true })`flyctl version`;

  if (result.exitCode !== 0) {
    throw new Error(
      [
        "Failed to get Fly CLI version. Most likely the Fly CLI is not installed.",
        "Read how to install the Fly CLI here: https://fly.io/docs/hands-on/install-flyctl",
      ].join("\n"),
    );
  }

  const match = result.stdout.match(/flyctl(?:\.exe)? v?(\S+)/);

  if (match === null) {
    throw new Error(
      `Failed to get Fly CLI version from output "${result.stdout.trim()}".`,
    );
  }

  return parseCliVersion("Fly CLI", match[1]);
}

export async function assertRegionIsValid(region: string): Promise<void> {
  const validRegion = await regionExists(region);
  if (!validRegion) {
    const flyRegionsCommand = `${getFullCommandName(executeFlyCommand)} platform regions --context server`;
    throw new Error(
      [
        `Invalid region code ${region}. Please specify a valid 3 character region id: https://fly.io/docs/reference/regions`,
        `You can also run "${flyRegionsCommand}".`,
      ].join("\n"),
    );
  }
}

async function regionExists(regionCode: string): Promise<boolean> {
  const flyCli = createCommand("flyctl");
  const regions = await runJsonCommand(
    flyCli,
    ["platform", "regions", "-j"],
    FlyRegionListSchema,
  );

  return regions.some((r) => {
    return r.code === regionCode;
  });
}

export async function secretExists(secretName: string): Promise<boolean> {
  const flyCli = createCommand("flyctl");
  const secrets = await runJsonCommand(
    flyCli,
    ["secrets", "list", "-j"],
    FlySecretListSchema,
  );
  return secrets.some((s) => s.name === secretName);
}
