import { Lucia, type Adapter, type DatabaseSession } from "lucia";
import { prisma } from '../index.js'
import type { User } from "../../entities/index.js"

// Ported from `@lucia-auth/adapter-prisma` because that package is deprecated
// and doesn't support Prisma 6:
// https://github.com/lucia-auth/lucia/blob/v3/packages/adapter-prisma/src/index.ts
const prismaAdapter: Adapter = {
  async getSessionAndUser(sessionId) {
    const session = await prisma.session.findUnique({
      where: { id: sessionId },
      include: { auth: true },
    });
    if (!session) {
      return [null, null];
    }
    const { auth: sessionAuth, ...sessionWithoutAuth } = session;
    return [
      toDatabaseSession(sessionWithoutAuth),
      { id: sessionAuth.id, attributes: { userId: sessionAuth.userId } },
    ];
  },
  async getUserSessions(userId) {
    const sessions = await prisma.session.findMany({ where: { userId } });
    return sessions.map(toDatabaseSession);
  },
  async setSession(session) {
    await prisma.session.create({
      data: {
        id: session.id,
        userId: session.userId,
        expiresAt: session.expiresAt,
      },
    });
  },
  async updateSessionExpiration(sessionId, expiresAt) {
    await prisma.session.update({
      where: { id: sessionId },
      data: { expiresAt },
    });
  },
  async deleteSession(sessionId) {
    // `deleteMany` doesn't throw if the session doesn't exist.
    await prisma.session.deleteMany({ where: { id: sessionId } });
  },
  async deleteUserSessions(userId) {
    await prisma.session.deleteMany({ where: { userId } });
  },
  async deleteExpiredSessions() {
    await prisma.session.deleteMany({
      where: { expiresAt: { lte: new Date() } },
    });
  },
};

function toDatabaseSession(session: {
  id: string;
  userId: string;
  expiresAt: Date;
}): DatabaseSession {
  return {
    id: session.id,
    userId: session.userId,
    expiresAt: session.expiresAt,
    attributes: {},
  };
}

// PRIVATE API
/**
 * We are using Lucia for session management.
 * 
 * Some details:
 * 1. We are using our own Prisma adapter for Lucia.
 * 2. We are not using cookies for session management. Instead, we are using
 *    the Authorization header to send the session token.
 * 3. Our `Session` entity is connected to the `Auth` entity.
 * 4. We are exposing the `userId` field from the `Auth` entity to
 *    make fetching the User easier.
 */
export const auth = new Lucia<{}, {
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
      userId: User['id'] | null
    };
  }
}
