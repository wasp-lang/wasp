{{={= =}=}}
import { Lucia } from "lucia";
import { PrismaAdapter } from "@lucia-auth/adapter-prisma";
import { prisma } from 'wasp/server'
import { type {= userEntityUpper =} } from "wasp/entities"
import { TimeSpan } from "./jwt.js"

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
 * 2. We are not using cookies for session management. Instead, we are using
 *    the Authorization header to send the session token.
 * 3. Our `Session` entity is connected to the `Auth` entity.
 * 4. We are exposing the `userId` field from the `Auth` entity to
 *    make fetching the User easier.
 */
export const auth = new Lucia<{}, {
  userId: {= userEntityUpper =}['id']
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
  sessionExpiresIn: new TimeSpan({= sessionExpiresInMs =}, "ms"),
  getUserAttributes({ userId }) {
    return {
      userId,
    };
  },
});

declare module "lucia" {
  interface Register {
    Lucia: typeof auth;
    DatabaseSessionAttributes: {};
    DatabaseUserAttributes: {
      userId: {= userEntityUpper =}['id']
    };
  }
}
