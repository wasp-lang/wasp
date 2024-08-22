{{={= =}=}}
import { GitHub  } from "arctic";

import { ensureEnvVarsForProvider } from "../env.js";
import { defineProvider } from "../provider.js";

const id = "{= providerId =}";
const displayName = "{= displayName =}";

const env = ensureEnvVarsForProvider(
  ["GITHUB_CLIENT_ID", "GITHUB_CLIENT_SECRET"],
  displayName
);

const oAuthClient = new GitHub(
  env.GITHUB_CLIENT_ID,
  env.GITHUB_CLIENT_SECRET,
);

// PUBLIC API
export const github = defineProvider({
  id,
  displayName,
  env,
  oAuthClient,
})