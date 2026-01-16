{{={= =}=}}
import { GitHub  } from 'arctic';

import { defineProvider } from '../../../../../core/server/auth/oauth/provider.js';
import { env } from '../../../env.js';

const id = '{= providerId =}';
const displayName = '{= displayName =}';

const oAuthClient = new GitHub(
  env.GITHUB_CLIENT_ID,
  env.GITHUB_CLIENT_SECRET,
);

// PUBLIC API
export const github = defineProvider({
  id,
  displayName,
  oAuthClient,
});
