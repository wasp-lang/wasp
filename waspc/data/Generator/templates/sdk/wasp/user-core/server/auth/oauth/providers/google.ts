{{={= =}=}}
import { Google  } from 'arctic';

import { getRedirectUriForCallback } from '../redirect';
import { defineProvider } from '../../../../../core/server/auth/oauth/provider';
import { env } from '../../../env';

const id = '{= providerId =}';
const displayName = '{= displayName =}';

const oAuthClient = new Google(
  env.GOOGLE_CLIENT_ID,
  env.GOOGLE_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);

// PUBLIC API
export const google = defineProvider({
  id,
  displayName,
  oAuthClient,
});
