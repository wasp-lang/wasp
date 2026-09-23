{{={= =}=}}
{=# isPrismaStoreUsed =}
import { Lucia } from "lucia";
import { PrismaAdapter } from "@lucia-auth/adapter-prisma";
import { prisma } from '../index.js'
import type { {= userEntityUpper =} } from "../../entities/index.js"

const prismaAdapter = new PrismaAdapter(
  prisma.{= sessionEntityLower =},
  prisma.{= authEntityLower =},
);

// PRIVATE API
/**
 * Lucia backs the `prisma` credential store. It knows nothing about
 * transports: the issuer decides whether the session id travels as a bearer
 * token or a cookie.
 */
export const auth = new Lucia<{
  loginScheme: string
  credentialScheme: string
}, {
  userId: {= userEntityUpper =}['id'] | null
}>(prismaAdapter, {
  getSessionAttributes({ loginScheme, credentialScheme }) {
    return { loginScheme, credentialScheme };
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
      loginScheme: string;
      // The scheme whose credential the row is.
      credentialScheme: string;
    };
    DatabaseUserAttributes: {
      userId: {= userEntityUpper =}['id'] | null
    };
  }
}
{=/ isPrismaStoreUsed =}
{=^ isPrismaStoreUsed =}
// No scheme uses the 'prisma' credential store; there is no Session model to
// back Lucia with. This module exists so the import graph is stable.
export {}
{=/ isPrismaStoreUsed =}
