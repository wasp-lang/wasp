import { createAuthClient } from "better-auth/client";
import { config } from "wasp/client";

/**
 * Better Auth's own client, pointed at the routes mounted in `routes.ts`.
 *
 * This is the honest shape of the deal: Wasp does not wrap Better Auth's login
 * API, so the login page below uses Better Auth's own methods. Only *reading*
 * the session is uniform across providers -- establishing one is not.
 */
export const authClient = createAuthClient({
  baseURL: `${config.apiUrl}/better-auth`,
});
