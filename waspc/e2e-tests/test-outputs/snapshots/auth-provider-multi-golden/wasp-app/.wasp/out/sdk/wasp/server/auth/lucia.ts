import { Lucia } from "lucia";
import { PrismaAdapter } from "@lucia-auth/adapter-prisma";
import { prisma } from '../index.js'
import type { User } from "../../entities/index.js"

const prismaAdapter = new PrismaAdapter(
  prisma.session,
  prisma.auth,
);

// PRIVATE API
/**
 * Lucia backs the `prisma` credential store. It knows nothing about
 * transports: the issuer decides whether the session id travels as a bearer
 * token or a cookie.
 */
export const auth = new Lucia<{
  signedInBy: string
}, {
  userId: User['id'] | null
}>(prismaAdapter, {
  getSessionAttributes({ signedInBy }) {
    return { signedInBy };
  },
  getUserAttributes({ userId }) {
    return { userId };
  },
});

declare module "lucia" {
  interface Register {
    Lucia: typeof auth;
    DatabaseSessionAttributes: {
      // The scheme that verified the login this credential descends from.
      signedInBy: string;
    };
    DatabaseUserAttributes: {
      userId: User['id'] | null
    };
  }
}
