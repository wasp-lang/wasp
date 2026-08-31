{{={= =}=}}
import { Keycloak  } from 'arctic';

import { getProviderCallbackUrl } from '../redirect.js';
import { defineProvider } from '../provider.js';
import { env } from '../../../env.js';

const id = '{= providerId =}';
const displayName = '{= displayName =}';

const oAuthClient = new Keycloak(
  env.KEYCLOAK_REALM_URL,
  env.KEYCLOAK_CLIENT_ID,
  env.KEYCLOAK_CLIENT_SECRET,
  getProviderCallbackUrl(id).toString(),
);

// PUBLIC API
export const keycloak = defineProvider({
  id,
  displayName,
  oAuthClient,
});
