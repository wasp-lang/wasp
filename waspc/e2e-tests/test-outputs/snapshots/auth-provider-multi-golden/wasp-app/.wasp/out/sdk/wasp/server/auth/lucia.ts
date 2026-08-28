import { Lucia } from "lucia";
import { PrismaAdapter } from "@lucia-auth/adapter-prisma";
import { prisma } from '../index.js'
import { type User } from "../../entities/index.js"

const prismaAdapter = new PrismaAdapter(
  prisma.session,
  prisma.auth,
);

// PRIVATE API
/**
 * We are using Lucia for session management.
 * 
 * Some details:
 * 1. We are using the Prisma adapter for Lucia.
 * 2. We are not using cookies for session management. Instead, we are using
 *    the Authorization header to send the session token.
 * 3. Our `Session` entity is connected to the `Auth` entity.
 * 4. We are exposing the `userId` field from the `Auth` entity to
 *    make fetching the User easier.
 */
export const auth = new Lucia<{
  providerId: string | null
  providerSessionId: string | null
}, {
  userId: User['id'] | null
}>(prismaAdapter, {
  // Since we are not using cookies, we don't need to set any cookie options.
  // But in the future, if we decide to use cookies, we can set them here.

  // sessionCookie: {
  //   name: "session",
  //   expires: true,
  //   attributes: {
  //     secure: !config.isDevelopment,
  //     sameSite: "lax",
  //   },
  // },
  getSessionAttributes({ providerId, providerSessionId }) {
    return {
      providerId,
      providerSessionId,
    };
  },
  getUserAttributes({ userId }) {
    return {
      userId,
    };
  },
});

declare module "lucia" {
  interface Register {
    Lucia: typeof auth;
    DatabaseSessionAttributes: {
      // Id of the provider that minted this session ('wasp', 'external:clerk',
      // ...); null only on rows from before the column existed.
      providerId: string | null;
      // The external provider's own session id when this session was minted by
      // credential exchange; lets logout revoke both sessions (dual sign-out).
      providerSessionId: string | null;
    };
    DatabaseUserAttributes: {
      userId: User['id'] | null
    };
  }
}
