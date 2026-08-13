import type {
  AuthProvider,
  VerifiedSession,
} from "wasp/server/auth/provider/types";

import { auth } from "./betterAuth";

/**
 * Better Auth, expressed as a Wasp `AuthProvider`.
 *
 * The whole adapter is two methods, and only the first one does real work.
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

  /**
   * Wasp hands every adapter a standard web `Request` -- built from the HTTP
   * request, or synthesized with just an `Authorization` header for websocket
   * auth. Better Auth consumes its headers directly either way.
   */
  async authenticate(request: Request): Promise<VerifiedSession | null> {
    const session = await auth.api.getSession({ headers: request.headers });

    if (!session) {
      return null;
    }

    return {
      sessionId: session.session.id,
      subjectId: session.user.id,
      // Verified profile data Wasp records when it provisions the local user.
      claims: {
        email: session.user.email,
        name: session.user.name,
      },
    };
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
