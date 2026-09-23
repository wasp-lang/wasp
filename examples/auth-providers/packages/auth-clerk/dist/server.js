import { createClerkClient } from "@clerk/backend";
/**
 * Clerk, expressed as a Wasp `CredentialHandler`.
 *
 * This is about the smallest possible handler, and Clerk is by far the least
 * work to integrate: it contributes **no Prisma models and no routes**. It
 * only ever answers "whose request is this?", from Clerk's own session token,
 * which the client auth handler puts on every request. Wasp issues nothing for it.
 *
 * It is also the handler that shows why `signIn` is optional on the
 * contract. Clerk has **no server-side password login at all** -- password
 * verification lives on its Frontend API behind a browser-held `__client`
 * cookie, and its Backend API has no endpoint that turns credentials into a
 * session. So no other scheme can sign into Clerk, and Clerk verifies no
 * login of its own on the server: it authenticates and signs out, and stops
 * there.
 *
 * Secrets come from `runtime.env`, already validated against the env vars the
 * manifest declared -- the handler never reads `process.env` itself.
 */
export const createServerAuthHandler = (runtime) => {
    const clerk = createClerkClient({
        secretKey: runtime.env.CLERK_SECRET_KEY,
        publishableKey: runtime.env.CLERK_PUBLISHABLE_KEY,
    });
    const jwtKey = runtime.env.CLERK_JWT_KEY;
    async function verify(request) {
        const requestState = await clerk.authenticateRequest(request, { jwtKey });
        if (!requestState.isAuthenticated) {
            return null;
        }
        const { userId, sessionId, sessionClaims } = requestState.toAuth();
        if (!userId || !sessionId) {
            return null;
        }
        return {
            userId,
            sessionId,
            issuedAt: new Date(sessionClaims.iat * 1000),
            claims: sessionClaims,
        };
    }
    const credentialHandler = {
        /**
         * Wasp hands every handler a standard web `Request` -- built from the
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
            const verified = await verify(request);
            if (verified === null) {
                return { status: "unauthenticated" };
            }
            return {
                status: "authenticated",
                principal: {
                    providerUserId: verified.userId,
                    credentialId: verified.sessionId,
                    // What lets Wasp's `signOutEverywhere` refuse this token after a
                    // cut-off, even before Clerk's own revocation lands.
                    credentialIssuedAt: verified.issuedAt,
                    // The verified JWT's claims, recorded by Wasp when it provisions the
                    // local user. NOTE: Clerk's default session token carries no email --
                    // add one to the token template in the Clerk dashboard if the app's
                    // user entity needs it at provisioning time.
                    claims: verified.claims,
                },
            };
        },
        /**
         * Clerk sessions are revocable server-side, which is what lets Wasp's
         * `logout()` stay uniform across schemes.
         *
         * Worth knowing: because Clerk's session tokens are short-lived JWTs
         * verified locally, revocation is not instantaneous -- an already-issued
         * token stays valid until it expires (~60s by default). Wasp's own
         * issuer revokes instantly. Same API, weaker guarantee.
         */
        async signOut(request) {
            const verified = await verify(request);
            if (verified !== null) {
                await clerk.sessions.revokeSession(verified.sessionId);
            }
            return Response.json({ success: true });
        },
        /** "Log out every device": revoke each of the user's active Clerk sessions. */
        async signOutEverywhere({ providerUserId }) {
            const { data: sessions } = await clerk.sessions.getSessionList({
                userId: providerUserId,
                status: "active",
            });
            for (const session of sessions) {
                await clerk.sessions.revokeSession(session.id);
            }
        },
    };
    return { credentialHandler };
};
