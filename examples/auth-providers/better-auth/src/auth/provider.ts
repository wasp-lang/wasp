import { fromNodeHeaders } from "better-auth/node";
import type { Request as ExpressRequest } from "express";
import type {
  AuthProvider,
  VerifiedSession,
} from "wasp/server/auth/provider/types";

import { auth } from "./betterAuth";

/**
 * Better Auth, expressed as a Wasp `AuthProvider`.
 *
 * The whole adapter is three methods, and only the first one does real work.
 * Everything Wasp builds on top of it -- `context.user`, `authRequired` pages,
 * `auth: true` operations, `useAuth()`, websocket auth -- comes for free.
 *
 * Note what is NOT here: no user creation, no notion of what a user record looks
 * like, no knowledge of this app's `User` entity. The adapter answers "whose
 * request is this?" and Wasp resolves that answer to a local row itself. That
 * separation is what keeps `context.user.id` meaning the same thing here as it
 * does in the Wasp-auth and Clerk examples.
 */
export const betterAuthProvider: AuthProvider = {
  /**
   * Becomes `AuthIdentity.providerName` for every user provisioned through this
   * adapter, so it must stay stable across deploys. Changing it orphans users.
   */
  id: "better-auth",

  async verifyRequest(req: ExpressRequest): Promise<VerifiedSession | null> {
    const session = await auth.api.getSession({
      headers: fromNodeHeaders(req.headers),
    });

    if (!session) {
      return null;
    }

    return { sessionId: session.session.id, subjectId: session.user.id };
  },

  /**
   * Websockets hand Wasp a bare token rather than a request, so the adapter has
   * to be able to verify one out of context. Better Auth reads the bearer token
   * from an `Authorization` header, so we synthesise the header it expects.
   */
  async verifyCredential(credential: string): Promise<VerifiedSession | null> {
    const session = await auth.api.getSession({
      headers: new Headers({ authorization: `Bearer ${credential}` }),
    });

    if (!session) {
      return null;
    }

    return { sessionId: session.session.id, subjectId: session.user.id };
  },

  async revokeSession(sessionId: string): Promise<void> {
    // Better Auth revokes by token rather than by id, so look it up first.
    const session = await auth.api.listSessions({ headers: new Headers() });
    const match = session?.find((s) => s.id === sessionId);
    if (match) {
      await auth.api.revokeSession({
        body: { token: match.token },
        headers: new Headers(),
      });
    }
  },
};
