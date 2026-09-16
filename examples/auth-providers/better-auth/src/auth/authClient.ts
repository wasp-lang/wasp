import { createBetterAuthClient } from "@wasp.sh/auth-better-auth/client";

/**
 * Better Auth's own client, pointed at the routes the adapter's manifest
 * mounted on the Wasp server (`/auth/better-auth`).
 *
 * This is the honest shape of the deal: Wasp does not wrap Better Auth's login
 * API, so the login page uses Better Auth's own methods. Only *reading* the
 * session is uniform across schemes -- establishing one is not. The client
 * hands each fresh session token to Wasp itself, so every Wasp API call
 * carries it.
 */
// NOTE: the explicit annotation matters. The inferred type would reference
// types nested inside the adapter package's own node_modules, which TypeScript
// rejects as non-portable when it builds the app's declarations (TS2883).
export const authClient: ReturnType<typeof createBetterAuthClient> =
  createBetterAuthClient();
