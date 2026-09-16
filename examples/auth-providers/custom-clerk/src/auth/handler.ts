import { createClerkClient } from "@clerk/backend";
import { getSchemeRuntime } from "wasp/server/auth";
import type {
  AuthenticateResult,
  AuthHandler,
  AuthResponse,
  Principal,
} from "wasp/server/auth/handler/types";

/**
 * Clerk, expressed as a Wasp `AuthHandler`.
 *
 * This app hand-writes the handler and registers it with `customAuthHandler()`
 * -- the escape hatch for providers nobody has packaged yet. Compare `../clerk`,
 * where the same handler ships as an npm package instead. Clerk is by far the
 * least work to integrate: it contributes **no Prisma models and no routes**. It
 * only ever answers "whose request is this?", from Clerk's own token, which the
 * app puts on every request (see `App.tsx`). Wasp issues nothing for it.
 *
 * It is also the handler that shows why `signIn` is optional on the contract.
 * Clerk has **no server-side password login at all** -- password verification
 * lives on its Frontend API behind a browser-held `__client` cookie, and its
 * Backend API has no endpoint that turns credentials into a session. So this
 * object authenticates and signs out, and stops there.
 *
 * Secrets come from the scheme's runtime, validated against the env vars the
 * manifest declared -- the same window a packaged handler gets.
 */
const clerk = lazy(() => {
  const { env } = getSchemeRuntime("clerk");
  return {
    client: createClerkClient({
      secretKey: env.CLERK_SECRET_KEY,
      publishableKey: env.CLERK_PUBLISHABLE_KEY,
    }),
    jwtKey: env.CLERK_JWT_KEY,
  };
});

async function verify(
  request: Request,
): Promise<{ userId: string; sessionId: string; claims: unknown } | null> {
  const { client, jwtKey } = clerk();
  const requestState = await client.authenticateRequest(request, { jwtKey });
  if (!requestState.isAuthenticated) {
    return null;
  }
  const { userId, sessionId, sessionClaims } = requestState.toAuth();
  if (!userId || !sessionId) {
    return null;
  }
  return { userId, sessionId, claims: sessionClaims };
}

export const clerkAuthHandler: AuthHandler = {
  /**
   * Wasp hands every handler a standard web `Request` -- built from the HTTP
   * request, or synthesized with just an `Authorization` header for websocket
   * auth. Clerk's SDK consumes one natively, so there is nothing to convert.
   *
   * Clerk reads either its `__session` cookie or an `Authorization: Bearer`
   * header transparently, so the same code serves web and native clients.
   *
   * With `jwtKey` set this is local RS256 verification with no network call;
   * without it, Clerk fetches (and caches) the JWKS.
   */
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
        // The verified JWT's claims, recorded by Wasp when it provisions the
        // local user. NOTE: Clerk's default session token carries no email --
        // add one to the token template in the Clerk dashboard if the app's
        // user entity needs it at provisioning time.
        claims: verified.claims as Principal["claims"],
      },
    };
  },

  /**
   * Clerk sessions are revocable server-side, which is what lets `logout()` stay
   * uniform across all the example apps.
   *
   * Worth knowing: because Clerk's session tokens are short-lived JWTs verified
   * locally, revocation is not instantaneous -- an already-issued token stays
   * valid until it expires (~60s by default). Wasp's own issuer revokes
   * instantly. Same API, weaker guarantee.
   */
  async signOut(request: Request): Promise<AuthResponse> {
    const verified = await verify(request);
    if (verified !== null) {
      await clerk().client.sessions.revokeSession(verified.sessionId);
    }
    return { status: 200, body: { success: true } };
  },
};

function lazy<T>(make: () => T): () => T {
  let value: T | undefined;
  return () => (value ??= make());
}
