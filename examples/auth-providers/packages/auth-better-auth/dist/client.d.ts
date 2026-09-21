import type { ClientAuthAdapterFor } from "@wasp.sh/auth-contract/client";
import type { betterAuth as betterAuthSpecHelper } from "./spec.js";
/**
 * The client half of the handler, instantiated by Wasp's generated client.
 *
 * It only captures the runtime window: the scheme's mount URL, and the
 * credential sink that stores the Better Auth session token so Wasp attaches
 * it to every request and routes `logout()` back to this scheme.
 */
export declare const createClientAuthHandler: ClientAuthAdapterFor<typeof betterAuthSpecHelper>;
/**
 * Better Auth's own client, pointed at the routes the manifest mounted on the
 * Wasp server.
 *
 * This is the honest shape of the deal: Wasp does not wrap Better Auth's login
 * API, so login pages use Better Auth's own methods
 * (`authClient.signIn.email(...)`). Only *reading* the session is uniform
 * across schemes -- establishing one is not.
 *
 * ```ts
 * import { createBetterAuthClient } from "@wasp.sh/auth-better-auth/client";
 *
 * export const authClient = createBetterAuthClient();
 * ```
 *
 * After each successful auth response the client hands the fresh bearer token
 * to Wasp, so every subsequent Wasp API call carries it and `useAuth()` sees
 * the login; the login page needs no hand-off of its own.
 */
export declare function createBetterAuthClient(): import("better-auth/client").AuthClient<{
    baseURL: string;
    fetchOptions: {
        onSuccess: (ctx: import("@better-fetch/fetch").SuccessContext<any>) => void;
    };
}>;
