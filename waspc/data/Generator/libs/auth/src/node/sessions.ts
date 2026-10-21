import { PrismaAdapter } from "@lucia-auth/adapter-prisma";
import type { Request as ExpressRequest } from "express";
import { Lucia, type Session } from "lucia";
import type { CompleteUserEntityWithAuth } from "../shared/authUser.js";
import type { AuthEntityTypes } from "../shared/entities.js";
import { createInvalidCredentialsError } from "../shared/errors.js";
import type { PrismaModelDelegate } from "./identities.js";

declare module "lucia" {
  interface Register {
    Lucia: Lucia<{}, { userId: unknown }>;
    DatabaseSessionAttributes: {};
    DatabaseUserAttributes: {
      // The app's user id type is only known to the generated code. The lib
      // only passes the id back into the app's own `findUnique`.
      userId: unknown;
    };
  }
}

/** The Prisma delegate shape `@lucia-auth/adapter-prisma` needs. */
export type LuciaPrismaModel = {
  fields: unknown;
  findUnique: unknown;
  findMany: unknown;
};

export type SessionsConfig<T extends AuthEntityTypes, AuthUserData> = {
  prisma: {
    session: LuciaPrismaModel;
    auth: LuciaPrismaModel;
    user: PrismaModelDelegate;
  };
  authFieldOnUserEntityName: T["authFieldOnUserEntity"];
  identitiesFieldOnAuthEntityName: T["identitiesFieldOnAuthEntity"];
  createAuthUserData: (user: CompleteUserEntityWithAuth<T>) => AuthUserData;
};

export type SessionAndUser<AuthUserData> = {
  session: Session;
  user: AuthUserData;
};

export type Sessions<AuthUserData> = {
  /** Creates a new session for the `authId` in the database. */
  createSession: (authId: string) => Promise<Session>;
  getSessionAndUserFromBearerToken: (
    req: ExpressRequest,
  ) => Promise<SessionAndUser<AuthUserData> | null>;
  getSessionAndUserFromSessionId: (
    sessionId: string,
  ) => Promise<SessionAndUser<AuthUserData> | null>;
  invalidateSession: (sessionId: string) => Promise<void>;
  /** Invalidates all sessions belonging to the `authId` in the database. */
  invalidateAllSessionsForAuthId: (authId: string) => Promise<void>;
};

/**
 * Creates the session functions, backed by Lucia.
 *
 * Some details:
 * 1. We are using the Prisma adapter for Lucia.
 * 2. We are not using cookies for session management. Instead, we are using
 *    the Authorization header to send the session token.
 * 3. Our `Session` entity is connected to the `Auth` entity.
 * 4. We are exposing the `userId` field from the `Auth` entity to
 *    make fetching the User easier.
 */
export function createSessions<T extends AuthEntityTypes, AuthUserData>(
  config: SessionsConfig<T, AuthUserData>,
): Sessions<AuthUserData> {
  const prismaAdapter = new PrismaAdapter(
    config.prisma.session,
    config.prisma.auth,
  );

  const lucia = new Lucia<{}, { userId: unknown }>(prismaAdapter, {
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

  async function createSession(authId: string): Promise<Session> {
    return lucia.createSession(authId, {});
  }

  async function getSessionAndUserFromBearerToken(
    req: ExpressRequest,
  ): Promise<SessionAndUser<AuthUserData> | null> {
    const authorizationHeader = req.headers["authorization"];

    if (typeof authorizationHeader !== "string") {
      return null;
    }

    const sessionId = lucia.readBearerToken(authorizationHeader);
    if (!sessionId) {
      return null;
    }

    return getSessionAndUserFromSessionId(sessionId);
  }

  async function getSessionAndUserFromSessionId(
    sessionId: string,
  ): Promise<SessionAndUser<AuthUserData> | null> {
    const { session, user: authEntity } =
      await lucia.validateSession(sessionId);

    if (!session || !authEntity) {
      return null;
    }

    // Such a session can't identify a user, so we treat it as unauthenticated.
    if (authEntity.userId === null) {
      return null;
    }

    return {
      session,
      user: await getAuthUserData(authEntity.userId),
    };
  }

  async function getAuthUserData(userId: unknown): Promise<AuthUserData> {
    const user: CompleteUserEntityWithAuth<T> | null =
      await config.prisma.user.findUnique({
        where: { id: userId },
        include: {
          [config.authFieldOnUserEntityName]: {
            include: {
              [config.identitiesFieldOnAuthEntityName]: true,
            },
          },
        },
      });

    if (!user) {
      throw createInvalidCredentialsError();
    }

    return config.createAuthUserData(user);
  }

  function invalidateSession(sessionId: string): Promise<void> {
    return lucia.invalidateSession(sessionId);
  }

  function invalidateAllSessionsForAuthId(authId: string): Promise<void> {
    return lucia.invalidateUserSessions(authId);
  }

  return {
    createSession,
    getSessionAndUserFromBearerToken,
    getSessionAndUserFromSessionId,
    invalidateSession,
    invalidateAllSessionsForAuthId,
  };
}
