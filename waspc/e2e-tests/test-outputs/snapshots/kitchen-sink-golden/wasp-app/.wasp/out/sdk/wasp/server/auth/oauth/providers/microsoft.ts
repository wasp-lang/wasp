import { MicrosoftEntraId } from 'arctic';

import { getRedirectUriForCallback } from '../redirect.js';
import { defineProvider } from '../provider.js';
import { env } from '../../../env.js';

const id = 'microsoft';
const displayName = 'Microsoft';

const oAuthClient = new MicrosoftEntraId(
  env.MICROSOFT_TENANT_ID,
  env.MICROSOFT_CLIENT_ID,
  env.MICROSOFT_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);

// PUBLIC API
export const microsoft = defineProvider({
  id,
  displayName,
  oAuthClient,
});
