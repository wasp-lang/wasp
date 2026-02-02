import { MicrosoftEntraId } from 'arctic';

import { getRedirectUriForCallback } from '../redirect.js';
import { defineProvider } from '../provider.js';
import { env } from '../../../env.js';

const id = 'microsoft-entra';
const displayName = 'Microsoft Entra';

const oAuthClient = new MicrosoftEntraId(
  env.MICROSOFT_ENTRA_TENANT_ID,
  env.MICROSOFT_ENTRA_CLIENT_ID,
  env.MICROSOFT_ENTRA_CLIENT_SECRET,
  getRedirectUriForCallback(id).toString(),
);

// PUBLIC API
export const microsoftEntra = defineProvider({
  id,
  displayName,
  oAuthClient,
});
