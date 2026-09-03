import { createClerkClient } from "@clerk/backend";
/**
 * Clerk, expressed as a Wasp `AuthProvider`.
 *
 * This is about the smallest possible adapter, and Clerk is by far the least
 * work to integrate: it contributes **no Prisma models and no routes**. It only
 * ever answers "whose request is this?".
 *
 * It is also the adapter that proves why session issuance is a separate
 * capability (`SupportsSessionIssuance`) rather than part of the base
 * interface. Clerk has **no
 * server-side password login at all** -- password verification lives on its
 * Frontend API behind a browser-held `__client` cookie, and its Backend API has
 * no endpoint that turns credentials into a session. So this adapter implements
 * `AuthProvider & SupportsSessionRevocation` and stops there: revocation yes, issuing no. A uniform `login(email, password)` could only
 * be implemented for Clerk as something that throws or silently ignores its
 * arguments; a missing capability is the honest alternative.
 *
 * Secrets come from `runtime.env`, already validated against the env vars the
 * manifest declared -- the adapter never reads `process.env` itself.
 */
export const createServerAdapter = (runtime) => {
    const clerk = createClerkClient({
        secretKey: runtime.env.CLERK_SECRET_KEY,
        publishableKey: runtime.env.CLERK_PUBLISHABLE_KEY,
    });
    const jwtKey = runtime.env.CLERK_JWT_KEY;
    const provider = {
        /**
         * Becomes `AuthIdentity.providerName`, so it must stay stable across
         * deploys and package versions.
         */
        id: "clerk",
        /**
         * Wasp hands every adapter a standard web `Request` -- built from the
         * HTTP request, or synthesized with just an `Authorization` header for
         * websocket auth. Clerk's SDK consumes one natively, so there is nothing
         * to convert.
         *
         * Clerk reads either its `__session` cookie or an `Authorization:
         * Bearer` header transparently, so the same code serves web and native
         * clients.
         *
         * With `CLERK_JWT_KEY` set this is local RS256 verification with no
         * network call; without it, Clerk fetches (and caches) the JWKS.
         */
        async authenticate(request) {
            const requestState = await clerk.authenticateRequest(request, {
                jwtKey,
            });
            if (!requestState.isAuthenticated) {
                return { status: "unauthenticated" };
            }
            const { userId, sessionId, sessionClaims } = requestState.toAuth();
            if (!userId || !sessionId) {
                return { status: "unauthenticated" };
            }
            return {
                status: "authenticated",
                session: {
                    sessionId,
                    subjectId: userId,
                    // The verified JWT's claims, recorded by Wasp when it provisions the
                    // local user. NOTE: Clerk's default session token carries no email --
                    // add one to the token template in the Clerk dashboard if the app's
                    // user entity needs it at provisioning time.
                    claims: sessionClaims,
                },
            };
        },
        /**
         * Clerk sessions are revocable server-side, which is what lets Wasp's
         * `logout()` stay uniform across providers.
         *
         * Worth knowing: because Clerk's session tokens are short-lived JWTs
         * verified locally, revocation is not instantaneous -- an already-issued
         * token stays valid until it expires (~60s by default). Wasp's own auth
         * revokes instantly. Same API, weaker guarantee.
         */
        async revokeSession(sessionId) {
            await clerk.sessions.revokeSession(sessionId);
        },
    };
    return { provider };
};
