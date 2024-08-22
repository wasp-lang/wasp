{{={= =}=}}
import { Discord  } from "arctic";

import { defineProvider } from "../provider.js";
import { ensureEnvVarsForProvider } from "../env.js";
import { getRedirectUriForCallback } from "../redirect.js";

const id = "{= providerId =}";
const displayName = "{= displayName =}";

const env = ensureEnvVarsForProvider(
  ["DISCORD_CLIENT_ID", "DISCORD_CLIENT_SECRET"],
  displayName
);

const oAuthClient = new Discord(
  env.DISCORD_CLIENT_ID,
  env.DISCORD_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);

// PUBLIC API
export const discord = defineProvider({
  id,
  displayName,
  env,
  oAuthClient,
})