{{={= =}=}}
import { Discord  } from "arctic";

import { ensureEnvVarsForProvider } from "../env.js";
import { getRedirectUriForCallback } from "../redirect.js";

export const id = "{= providerId =}";
export const displayName = "{= displayName =}";

const env = ensureEnvVarsForProvider(
  ["DISCORD_CLIENT_ID", "DISCORD_CLIENT_SECRET"],
  displayName
);

export const oAuthClient = new Discord(
  env.DISCORD_CLIENT_ID,
  env.DISCORD_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);
