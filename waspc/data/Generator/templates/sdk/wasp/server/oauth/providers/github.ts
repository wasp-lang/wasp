{{={= =}=}}
import { GitHub  } from "arctic";

import { ensureEnvVarsForProvider } from "../env.js";

export const id = "{= providerId =}";
export const displayName = "{= displayName =}";

const env = ensureEnvVarsForProvider(
  ["GITHUB_CLIENT_ID", "GITHUB_CLIENT_SECRET"],
  displayName
);

export const oAuthClient = new GitHub(
  env.GITHUB_CLIENT_ID,
  env.GITHUB_CLIENT_SECRET,
);