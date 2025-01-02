{{={= =}=}}
import { Twitter  } from "arctic";

import { defineProvider } from "../provider.js";
import { ensureEnvVarsForProvider } from "../env.js";
import { getRedirectUriForCallback } from "../redirect.js";

const id = "{= providerId =}";
const displayName = "{= displayName =}";

const env = ensureEnvVarsForProvider(
  ["TWITTER_CLIENT_ID", "TWITTER_CLIENT_SECRET"],
  displayName
);

const oAuthClient = new Twitter(
  env.TWITTER_CLIENT_ID,
  env.TWITTER_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);

// PUBLIC API
export const twitter = defineProvider({
  id,
  displayName,
  env,
  oAuthClient,
});
