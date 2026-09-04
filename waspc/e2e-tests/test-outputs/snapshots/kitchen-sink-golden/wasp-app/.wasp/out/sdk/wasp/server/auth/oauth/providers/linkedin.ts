import { LinkedIn } from 'arctic';

import { defineProvider } from '../provider.js';
import { getRedirectUriForCallback } from '../redirect.js';
import { env } from '../../../env.js';

const id = 'linkedin';
const displayName = 'LinkedIn';

const oAuthClient = new LinkedIn(
  env.LINKEDIN_CLIENT_ID,
  env.LINKEDIN_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);

// PUBLIC API
export const linkedin = defineProvider({
  id,
  displayName,
  oAuthClient,
});
