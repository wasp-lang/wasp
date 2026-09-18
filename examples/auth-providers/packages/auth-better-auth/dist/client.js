import { createAuthClient } from "better-auth/client";
let runtime = null;
/**
 * The client half of the handler, instantiated by Wasp's generated client.
 *
 * It only captures the runtime window: the scheme's mount URL, and the
 * credential sink that stores the Better Auth session token so Wasp attaches
 * it to every request and routes `logout()` back to this scheme.
 */
export const createClientAuthHandler = (newRuntime) => {
    runtime = newRuntime;
    return {};
};
function getRuntime() {
    if (runtime === null) {
        throw new Error("Better Auth's client used before Wasp instantiated it. Is betterAuth() among app.auth.schemes?");
    }
    return runtime;
}
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
export function createBetterAuthClient() {
    return createAuthClient({
        baseURL: getRuntime().mountUrl,
        fetchOptions: {
            onSuccess: (ctx) => {
                // The bearer plugin echoes the session token in this header on
                // successful auth responses.
                const token = ctx.response.headers.get("set-auth-token");
                if (token) {
                    void getRuntime().setCredential(token);
                }
            },
        },
    });
}
