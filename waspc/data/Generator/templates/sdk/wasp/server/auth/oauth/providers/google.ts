{{={= =}=}}
import { Google  } from "arctic";

import { ensureEnvVarsForProvider } from "../env.js";
import { getRedirectUriForCallback } from "../redirect.js";
import { defineProvider } from "../provider.js";

const id = "{= providerId =}";
const displayName = "{= displayName =}";

const env = ensureEnvVarsForProvider(
  ["GOOGLE_CLIENT_ID", "GOOGLE_CLIENT_SECRET"],
  displayName,
);

const oAuthClient = new Google(
  env.GOOGLE_CLIENT_ID,
  env.GOOGLE_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);

// PUBLIC API
export const google = defineProvider({
  id,
  displayName,
  env,
  oAuthClient,
});
