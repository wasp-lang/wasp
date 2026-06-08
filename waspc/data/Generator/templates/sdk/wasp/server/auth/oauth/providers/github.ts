{{={= =}=}}
import { GitHub  } from 'arctic';

import { defineProvider } from '../provider.js';
import { env } from '../../../env.js';

const id = '{= providerId =}';
const displayName = '{= displayName =}';

const oAuthClient = new GitHub(
  env.GITHUB_CLIENT_ID,
  env.GITHUB_CLIENT_SECRET,
  // The redirect URI is configured in the GitHub app, so we don't need to
  // pass it here. Since Arctic v2 the parameter is required, so we pass null.
  null,
);

// PUBLIC API
export const github = defineProvider({
  id,
  displayName,
  oAuthClient,
});
