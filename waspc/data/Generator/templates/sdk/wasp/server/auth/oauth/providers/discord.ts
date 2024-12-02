{{={= =}=}}
import { Discord  } from 'arctic';

import { defineProvider } from '../provider.js';
import { getRedirectUriForCallback } from '../redirect.js';
import { env } from '../../../env.js';

const id = '{= providerId =}';
const displayName = '{= displayName =}';

const oAuthClient = new Discord(
  env.DISCORD_CLIENT_ID,
  env.DISCORD_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);

// PUBLIC API
export const discord = defineProvider({
  id,
  displayName,
  oAuthClient,
});
