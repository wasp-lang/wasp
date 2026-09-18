import { createClerkClient } from "@clerk/backend";
import type {
  AuthenticateResult,
  AuthResponse,
  Principal,
  ServerAuthHandlerFactory,
} from "wasp/server/auth/handler/types";

/**
 * Clerk's server half, hand-written in the app and registered with
 * `customAuthHandler()` -- the escape hatch for providers nobody has packaged
 * yet. Compare `../clerk`, where the same thing ships as an npm package.
 *
 * It is a `ServerAuthHandlerFactory`, the function a handler package exports as
 * `createServerAuthHandler`, so it has the same powers: the scheme's runtime
 * arrives as an argument, with exactly the env vars the manifest declared.
 * Pasting this file into a package needs no edits.
 *
 * Clerk is the least work to integrate: no Prisma models and no routes. It
 * only answers "whose request is this?", from Clerk's own token, which the
 * client half puts on every request. Wasp issues nothing for it, so the
 * manifest declares no `credentials`.
 *
 * It is also why `signIn` is optional on the contract. Clerk has **no
 * server-side password login at all**: verification lives on its Frontend API
 * behind a browser-held cookie, and its Backend API cannot turn credentials
 * into a session. So this handler authenticates and signs out, and stops.
 */
export const createClerkServerAuthHandler: ServerAuthHandlerFactory = (
  runtime,
) => {
  const clerk = createClerkClient({
    secretKey: runtime.env.CLERK_SECRET_KEY,
    publishableKey: runtime.env.CLERK_PUBLISHABLE_KEY,
  });
  const jwtKey = runtime.env.CLERK_JWT_KEY;

  async function verify(
    request: Request,
  ): Promise<{ userId: string; sessionId: string; claims: unknown } | null> {
    // With `jwtKey` set this is local RS256 verification with no network
    // call; without it, Clerk fetches (and caches) the JWKS. Clerk reads its
    // `__session` cookie or an `Authorization: Bearer` header transparently.
    const requestState = await clerk.authenticateRequest(request, { jwtKey });
    if (!requestState.isAuthenticated) {
      return null;
    }
    const { userId, sessionId, sessionClaims } = requestState.toAuth();
    if (!userId || !sessionId) {
      return null;
    }
    return { userId, sessionId, claims: sessionClaims };
  }

  return {
    handler: {
      async authenticate(request: Request): Promise<AuthenticateResult> {
        const verified = await verify(request);
        if (verified === null) {
          return { status: "unauthenticated" };
        }
        return {
          status: "authenticated",
          principal: {
            subjectId: verified.userId,
            credentialId: verified.sessionId,
            // Recorded by Wasp when it provisions the local user. NOTE:
            // Clerk's default session token carries no email -- add one to
            // the token template in the Clerk dashboard if the app's user
            // entity needs it at provisioning time.
            claims: verified.claims as Principal["claims"],
          },
        };
      },

      /**
       * Revocable server-side, which keeps `logout()` uniform. Because
       * Clerk's tokens are short-lived JWTs verified locally, an already
       * issued token stays valid until it expires (~60s). Wasp's own issuer
       * revokes instantly. Same API, weaker guarantee.
       */
      async signOut(request: Request): Promise<AuthResponse> {
        const verified = await verify(request);
        if (verified !== null) {
          await clerk.sessions.revokeSession(verified.sessionId);
        }
        return { status: 200, body: { success: true } };
      },
    },
  };
};
