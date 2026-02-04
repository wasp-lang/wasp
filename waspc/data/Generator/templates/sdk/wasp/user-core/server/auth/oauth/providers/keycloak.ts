{{={= =}=}}
import { Keycloak  } from 'arctic';

import { getRedirectUriForCallback } from '../redirect';
import { defineProvider } from '../../../../../core/server/auth/oauth/provider';
import { env } from '../../../env';

const id = '{= providerId =}';
const displayName = '{= displayName =}';

const oAuthClient = new Keycloak(
  env.KEYCLOAK_REALM_URL,
  env.KEYCLOAK_CLIENT_ID,
  env.KEYCLOAK_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);

// PUBLIC API
export const keycloak = defineProvider({
  id,
  displayName,
  oAuthClient,
});
