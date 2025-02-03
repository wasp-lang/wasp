{{={= =}=}}
import { Lucia, TimeSpan } from "lucia";
import { PrismaAdapter } from "@lucia-auth/adapter-prisma";
import { prisma, config } from 'wasp/server'
import { type {= userEntityUpper =} } from "wasp/entities"

const prismaAdapter = new PrismaAdapter(
  prisma.{= sessionEntityLower =},
  prisma.{= authEntityLower =},
);

// PRIVATE API
/**
 * We are using Lucia for session management.
 * 
 * Some details:
 * 1. We are using the Prisma adapter for Lucia.
 * 2. We can use cookies for session management or we can use
 *    the Authorization header to send the session token.
 * 3. Our `Session` entity is connected to the `Auth` entity.
 * 4. We are exposing the `userId` field from the `Auth` entity to
 *    make fetching the User easier.
 */
{=# isCookieAuthEnabled =}
export const auth = new Lucia<{}, {
  userId: {= userEntityUpper =}['id']
}>(prismaAdapter, {
  sessionExpiresIn: new TimeSpan(config.sessionExpiresIn, "s"),
  sessionCookie: {
    name: config.cookieName,
    expires: config.cookieExpires,
    attributes: {
      path: config.cookiePath,
      sameSite: config.cookieSameSite,
      secure: config.cookieSecure
    }
  },
  getUserAttributes({ userId }) {
    return {
      userId,
    };
  },
});
{=/ isCookieAuthEnabled =}

{=^ isCookieAuthEnabled =}
export const auth = new Lucia<{}, {
  userId: {= userEntityUpper =}['id']
}>(prismaAdapter, {
  getUserAttributes({ userId }) {
    return {
      userId,
    };
  },
});
{=/ isCookieAuthEnabled =}

declare module "lucia" {
  interface Register {
    Lucia: typeof auth;
    DatabaseSessionAttributes: {};
    DatabaseUserAttributes: {
      userId: {= userEntityUpper =}['id']
    };
  }
}
