import { Google  } from 'arctic';

import { getRedirectUriForCallback } from '../redirect.js';
import { defineProvider } from '../provider.js';
import { env } from '../../../env.js';

const id = 'google';
const displayName = 'Google';

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
