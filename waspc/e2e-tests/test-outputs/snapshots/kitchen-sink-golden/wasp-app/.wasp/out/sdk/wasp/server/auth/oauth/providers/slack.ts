import { Slack } from 'arctic';

import { defineProvider } from '../provider.js';
import { getProviderCallbackUrl } from '../redirect.js';
import { env } from '../../../env.js';

const id = 'slack';
const displayName = 'Slack';

const oAuthClient = new Slack(
  env.SLACK_CLIENT_ID,
  env.SLACK_CLIENT_SECRET,
  getProviderCallbackUrl(id).toString(),
);

// PUBLIC API
export const slack = defineProvider({
  id,
  displayName,
  oAuthClient,
});
