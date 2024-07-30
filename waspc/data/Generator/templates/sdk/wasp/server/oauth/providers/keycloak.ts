{{={= =}=}}
import { Keycloak  } from "arctic";

import { ensureEnvVarsForProvider } from "../env.js";
import { getRedirectUriForCallback } from "../redirect.js";

export const id = "{= providerId =}";
export const displayName = "{= displayName =}";

const env = ensureEnvVarsForProvider(
  ["KEYCLOAK_REALM_URL", "KEYCLOAK_CLIENT_ID", "KEYCLOAK_CLIENT_SECRET"],
  displayName,
);

export const oAuthClient = new Keycloak(
  env.KEYCLOAK_REALM_URL,
  env.KEYCLOAK_CLIENT_ID,
  env.KEYCLOAK_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);