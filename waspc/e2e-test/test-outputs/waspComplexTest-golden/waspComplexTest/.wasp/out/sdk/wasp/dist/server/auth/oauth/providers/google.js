import { Google } from "arctic";
import { ensureEnvVarsForProvider } from "../env.js";
import { getRedirectUriForCallback } from "../redirect.js";
export const id = "google";
export const displayName = "Google";
const env = ensureEnvVarsForProvider(["GOOGLE_CLIENT_ID", "GOOGLE_CLIENT_SECRET"], displayName);
export const oAuthClient = new Google(env.GOOGLE_CLIENT_ID, env.GOOGLE_CLIENT_SECRET, getRedirectUriForCallback(id).toString());
//# sourceMappingURL=google.js.map