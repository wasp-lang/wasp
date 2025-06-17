import { confirm } from "@inquirer/prompts";
import { exit } from "process";
import { $ } from "zx";
import { getCommandName } from "../../../common/commander.js";
import { waspSays } from "../../../common/output.js";
import { executeFlyCommand } from "../index.js";
import {
  FlyRegionListSchema,
  FlySecretListSchema,
} from "./jsonOutputSchemas.js";

export async function flyctlExists(): Promise<boolean> {
  try {
    await $`flyctl version`;
    return true;
  } catch {
    return false;
  }
}

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
    waspSays("Ok, exiting.");
    exit(1);
  }

  try {
    await $`flyctl auth login`;
  } catch {
    waspSays(
      'It seems there was a problem logging in. Please run "flyctl auth login" and try again.',
    );
    exit(1);
  }
}

export async function ensureFlyReady(): Promise<void> {
  const doesFlyctlExist = await flyctlExists();
  if (!doesFlyctlExist) {
    waspSays("The Fly.io CLI is not available on this system.");
    waspSays(
      "Please install the flyctl here: https://fly.io/docs/hands-on/install-flyctl",
    );
    exit(1);
  }
  await ensureUserLoggedIn();
}

export async function ensureRegionIsValid(region: string): Promise<void> {
  try {
    const validRegion = await regionExists(region);
    if (!validRegion) {
      waspSays(
        `Invalid region code ${region}. Please specify a valid 3 character region id: https://fly.io/docs/reference/regions`,
      );
      waspSays(
        `You can also run "${getCommandName(executeFlyCommand).replace(
          "<cmd...>",
          "platform regions --context server",
        )}".`,
      );
      exit(1);
    }
  } catch (e) {
    // Ignore any errors while checking. Commands requiring a valid region will still fail if invalid, just not as nicely.
    waspSays("Unable to validate region before calling flyctl.");
  }
}

async function regionExists(regionCode: string): Promise<boolean> {
  const proc = await $`flyctl platform regions -j`.verbose(false);
  const regions = FlyRegionListSchema.parse(JSON.parse(proc.stdout));
  return regions.some((r) => {
    const code = "code" in r ? r.code : r.Code;
    return code === regionCode;
  });
}

export async function secretExists(secretName: string): Promise<boolean> {
  const proc = await $`flyctl secrets list -j`;
  const secrets = FlySecretListSchema.parse(JSON.parse(proc.stdout));
  return secrets.some((s) => {
    const name = "name" in s ? s.name : s.Name;
    return name === secretName;
  });
}
