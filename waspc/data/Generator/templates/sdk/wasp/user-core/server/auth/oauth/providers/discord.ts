{{={= =}=}}
import { Discord  } from 'arctic';

import { defineProvider } from '../../../../../core/server/auth/oauth/provider';
import { getRedirectUriForCallback } from '../redirect';
import { env } from '../../../env';

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
