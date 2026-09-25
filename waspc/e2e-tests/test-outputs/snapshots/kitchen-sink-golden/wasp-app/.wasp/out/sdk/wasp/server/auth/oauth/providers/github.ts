import { GitHub  } from 'arctic';

import { defineProvider } from '@wasp.sh/lib-sdk-core/node';
import { env } from '../../../env.js';

const id = 'github';
const displayName = 'GitHub';

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
