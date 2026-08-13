import { createBetterAuthClient } from "@wasp.sh/auth-better-auth/client";
import { config } from "wasp/client";

/**
 * Better Auth's own client, pointed at the routes the adapter's manifest
 * mounted on the Wasp server.
 *
 * This is the honest shape of the deal: Wasp does not wrap Better Auth's login
 * API, so the login page uses Better Auth's own methods. Only *reading* the
 * session is uniform across providers -- establishing one is not.
 */
// NOTE: the explicit annotation matters. The inferred type would reference
// types nested inside the adapter package's own node_modules, which TypeScript
// rejects as non-portable when it builds the app's declarations (TS2883).
export const authClient: ReturnType<typeof createBetterAuthClient> =
  createBetterAuthClient(config.apiUrl);
