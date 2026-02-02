import { confirm } from "@inquirer/prompts";
import { $ } from "zx";
import { getFullCommandName } from "../../common/commander.js";
import { executeFlyCommand } from "./index.js";
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
  const doesFlyctlExist = await flyctlExists();
  if (!doesFlyctlExist) {
    throw new Error(
      `The Fly.io CLI is not available on this system.\nPlease install the flyctl here: https://fly.io/docs/hands-on/install-flyctl`,
    );
  }
  await ensureUserLoggedIn();
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
  const proc = await $`flyctl platform regions -j`.verbose(false);
  const regions = FlyRegionListSchema.parse(proc.json());

  return regions.some((r) => {
    return r.code === regionCode;
  });
}

export async function secretExists(secretName: string): Promise<boolean> {
  const proc = await $`flyctl secrets list -j`;
  const secrets = FlySecretListSchema.parse(proc.json());
  return secrets.some((s) => {
    const name = "name" in s ? s.name : s.Name;
    return name === secretName;
  });
}
