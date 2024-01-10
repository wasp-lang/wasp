{{={= =}=}}
import { Lucia } from "lucia";
import { PrismaAdapter } from "@lucia-auth/adapter-prisma";
import prisma from '../dbClient.js'
import config from '../config.js'
import { type {= userEntityUpper =} } from "../entities/index.js"

const prismaAdapter = new PrismaAdapter(
  // Using `as any` here since Lucia's model types are not compatible with Prisma 4
  // model types. This is a temporary workaround until we migrate to Prisma 5.
  // This **works** in runtime, but Typescript complains about it.
  prisma.{= sessionEntityLower =} as any,
  prisma.{= authEntityLower =} as any
);

// TODO: figure out CSRF protection in Lucia - https://v3.lucia-auth.com/guides/validate-session-cookies/express/
export const auth = new Lucia<{}, {
  userId: {= userEntityUpper =}['id']
}>(prismaAdapter, {
  sessionCookie: {
    name: "session",
    expires: true,
    attributes: {
      secure: !config.isDevelopment,
      sameSite: "lax",
    },
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
    DatabaseSessionAttributes: {};
    DatabaseUserAttributes: {
      userId: {= userEntityUpper =}['id']
    };
  }
}
