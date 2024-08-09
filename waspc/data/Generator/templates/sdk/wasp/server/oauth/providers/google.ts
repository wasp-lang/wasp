{{={= =}=}}
import { Google  } from "arctic";

import { ensureEnvVarsForProvider } from "../env.js";
import { getRedirectUriForCallback } from "../redirect.js";

export const id = "{= providerId =}";
export const displayName = "{= displayName =}";

const env = ensureEnvVarsForProvider(
  ["GOOGLE_CLIENT_ID", "GOOGLE_CLIENT_SECRET"],
  displayName,
);

export const oAuthClient = new Google(
  env.GOOGLE_CLIENT_ID,
  env.GOOGLE_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);
