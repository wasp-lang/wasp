import { Request as ExpressRequest } from "express";

import { type User } from '../entities/index.js';
import { type AuthUserData } from '../server/auth/user.js';

import { auth } from "./lucia.js";
import type { Session } from "lucia";
import { throwInvalidCredentialsError } from "./utils.js";

import { prisma } from 'wasp/server';
import { createAuthUser } from "../server/auth/user.js";

// PRIVATE API
// Creates a new session for the `authId` in the database
export async function createSession(authId: string): Promise<Session> {
  return auth.createSession(authId, {});
}

// PRIVATE API
export async function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<{
  user: AuthUserData | null,
  session: Session | null,
}> {
  const authorizationHeader = req.headers["authorization"];

  if (typeof authorizationHeader !== "string") {
    return {
      user: null,
      session: null,
    };
  }

  const sessionId = auth.readBearerToken(authorizationHeader);
  if (!sessionId) {
    return {
      user: null,
      session: null,
    };
  }

  return getSessionAndUserFromSessionId(sessionId);
}

// PRIVATE API
export async function getSessionAndUserFromSessionId(sessionId: string): Promise<{
  user: AuthUserData | null,
  session: Session | null,
}> {
  const { session, user: authEntity } = await auth.validateSession(sessionId);

  if (!session || !authEntity) {
    return {
      user: null,
      session: null,
    };
  }

  return {
    session,
    user: await getUser(authEntity.userId)
  }
}

async function getUser(userId: User['id']): Promise<AuthUserData> {
  const user = await prisma.user
    .findUnique({
      where: { id: userId },
      include: {
        auth: {
          include: {
            identities: true
          }
        }
      }
    })

  if (!user) {
    throwInvalidCredentialsError()
  }

  return createAuthUser(user);
}

// PRIVATE API
export function invalidateSession(sessionId: string): Promise<void> {
  return auth.invalidateSession(sessionId);
}
