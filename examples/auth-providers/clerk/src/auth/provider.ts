import { createClerkClient } from "@clerk/backend";
import type { Request as ExpressRequest } from "express";
import type {
  AuthProvider,
  VerifiedSession,
} from "wasp/server/auth/provider/types";

const clerk = createClerkClient({
  secretKey: process.env.CLERK_SECRET_KEY,
  publishableKey: process.env.CLERK_PUBLISHABLE_KEY,
});

/**
 * Clerk, expressed as a Wasp `AuthProvider`.
 *
 * This is the smallest of the three adapters, and Clerk is by far the least
 * work to integrate: it contributes **no Prisma models and no routes**. It only
 * ever answers "whose request is this?".
 *
 * It is also the adapter that proves why `AuthProvider` and
 * `SessionIssuingAuthProvider` are separate interfaces. Clerk has **no
 * server-side password login at all** -- password verification lives on its
 * Frontend API behind a browser-held `__client` cookie, and its Backend API has
 * no endpoint that turns credentials into a session. So this object implements
 * `AuthProvider` and stops there. A uniform `login(email, password)` could only
 * be implemented for Clerk as something that throws or silently ignores its
 * arguments; a missing capability is the honest alternative.
 */
export const clerkAuthProvider: AuthProvider = {
  /**
   * Becomes `AuthIdentity.providerName`, so it must stay stable across deploys.
   */
  id: "clerk",

  async verifyRequest(req: ExpressRequest): Promise<VerifiedSession | null> {
    // Clerk reads either its `__session` cookie or an `Authorization: Bearer`
    // header transparently, so the same code serves web and native clients.
    //
    // With `jwtKey` set this is local RS256 verification with no network call;
    // without it, Clerk fetches (and caches) the JWKS.
    const requestState = await clerk.authenticateRequest(toWebRequest(req), {
      jwtKey: process.env.CLERK_JWT_KEY,
    });

    if (!requestState.isAuthenticated) {
      return null;
    }

    const { userId, sessionId } = requestState.toAuth();
    if (!userId || !sessionId) {
      return null;
    }

    return { sessionId, subjectId: userId };
  },

  /**
   * Websockets hand Wasp a bare token, so we rebuild the request Clerk expects.
   */
  async verifyCredential(credential: string): Promise<VerifiedSession | null> {
    const request = new Request("http://localhost/", {
      headers: { authorization: `Bearer ${credential}` },
    });

    const requestState = await clerk.authenticateRequest(request, {
      jwtKey: process.env.CLERK_JWT_KEY,
    });

    if (!requestState.isAuthenticated) {
      return null;
    }

    const { userId, sessionId } = requestState.toAuth();
    if (!userId || !sessionId) {
      return null;
    }

    return { sessionId, subjectId: userId };
  },

  /**
   * Clerk sessions are revocable server-side, which is what lets `logout()` stay
   * uniform across all three example apps.
   *
   * Worth knowing: because Clerk's session tokens are short-lived JWTs verified
   * locally, revocation is not instantaneous -- an already-issued token stays
   * valid until it expires (~60s by default). Wasp's own auth revokes instantly.
   * Same API, weaker guarantee.
   */
  async revokeSession(sessionId: string): Promise<void> {
    await clerk.sessions.revokeSession(sessionId);
  },
};

/** Express gives us a Node request; Clerk's SDK wants a web `Request`. */
function toWebRequest(req: ExpressRequest): Request {
  const headers = new Headers();
  for (const [key, value] of Object.entries(req.headers)) {
    if (typeof value === "string") {
      headers.set(key, value);
    } else if (Array.isArray(value)) {
      headers.set(key, value.join(", "));
    }
  }

  const host = req.get("host") ?? "localhost";
  return new Request(`${req.protocol}://${host}${req.originalUrl}`, {
    method: req.method,
    headers,
  });
}
