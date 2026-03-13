import { Request as ExpressRequest } from "express";

import { type User } from '../entities/index.js';
import { type AuthUserData } from '../server/auth/user.js';

import { auth } from "./lucia.js";
import type { Session } from "lucia";
import { createInvalidCredentialsError } from "./utils.js";

import { prisma } from 'wasp/server';
import { createAuthUserData } from "../server/auth/user.js";

// PRIVATE API
// Creates a new session for the `authId` in the database
export async function createSession(authId: string): Promise<Session> {
  return auth.createSession(authId, {});
}

type SessionAndUser = {
  session: Session;
  user: AuthUserData;
}

// PRIVATE API
export async function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<SessionAndUser | null> {
  const authorizationHeader = req.headers["authorization"];

  if (typeof authorizationHeader !== "string") {
    return null;
  }

  const sessionId = auth.readBearerToken(authorizationHeader);
  if (!sessionId) {
    return null;
  }

  return getSessionAndUserFromSessionId(sessionId);
}

// PRIVATE API
export async function getSessionAndUserFromSessionId(sessionId: string): Promise<SessionAndUser | null> {
  const { session, user: authEntity } = await auth.validateSession(sessionId);

  if (!session || !authEntity) {
    return null;
  }

  return {
    session,
    user: await getAuthUserData(authEntity.userId)
  }
}

async function getAuthUserData(userId: User['id']): Promise<AuthUserData> {
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
    throw createInvalidCredentialsError()
  }

  return createAuthUserData(user);
}

// PRIVATE API
export function invalidateSession(sessionId: string): Promise<void> {
  return auth.invalidateSession(sessionId);
}
