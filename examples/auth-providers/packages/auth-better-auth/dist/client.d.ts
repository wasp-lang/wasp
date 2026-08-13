/**
 * Better Auth's own client, pointed at the routes the manifest mounted on the
 * Wasp server.
 *
 * This is the honest shape of the deal: Wasp does not wrap Better Auth's login
 * API, so login pages use Better Auth's own methods
 * (`authClient.signIn.email(...)`). Only *reading* the session is uniform
 * across providers -- establishing one is not.
 *
 * ```ts
 * import { createBetterAuthClient } from "@wasp.sh/auth-better-auth/client";
 * import { config } from "wasp/client";
 *
 * export const authClient = createBetterAuthClient(config.apiUrl);
 * ```
 *
 * After a successful sign-in, hand the returned token to Wasp
 * (`setSessionId(result.data.token)` from `wasp/client/api`) so every
 * subsequent Wasp API call carries it. Or pass `onToken` here and the client
 * calls it with the fresh bearer token after each successful auth response --
 * `createBetterAuthClient(config.apiUrl, { onToken: setSessionId })` -- and
 * the login page needs no hand-off of its own.
 *
 * @param serverUrl The Wasp server's URL (`config.apiUrl` from `wasp/client`).
 */
export declare function createBetterAuthClient(serverUrl: string, options?: {
    onToken?: (token: string) => void;
}): import("better-auth/client").AuthClient<{
    fetchOptions?: {
        onSuccess: (ctx: import("@better-fetch/fetch").SuccessContext<any>) => void;
    } | undefined;
    baseURL: string;
}>;
