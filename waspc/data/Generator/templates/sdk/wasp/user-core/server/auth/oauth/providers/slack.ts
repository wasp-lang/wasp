{{={= =}=}}
import { Slack } from 'arctic';

import { defineProvider } from '../../../../../core/server/auth/oauth/provider.js';
import { getRedirectUriForCallback } from '../redirect.js';
import { env } from '../../../env.js';

const id = '{= providerId =}';
const displayName = '{= displayName =}';

const oAuthClient = new Slack(
  env.SLACK_CLIENT_ID,
  env.SLACK_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);

// PUBLIC API
export const slack = defineProvider({
  id,
  displayName,
  oAuthClient,
});
