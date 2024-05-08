{{={= =}=}}
import { Request as ExpressRequest } from "express";

import { type {= userEntityUpper =} } from '../entities/index.js';
import { type AuthUserData } from '../server/auth/user.js';

import { auth } from "./lucia.js";
import type { Session } from "lucia";
import { throwInvalidCredentialsError } from "./utils.js";

import { prisma } from 'wasp/server';
import { createAuthUserData } from "../server/auth/user.js";

// PRIVATE API
// Creates a new session for the `authId` in the database
export async function createSession(authId: string): Promise<Session> {
  return auth.createSession(authId, {});
}

type UserAndSession = {
  user: AuthUserData;
  session: Session;
}

// PRIVATE API
export async function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<UserAndSession | null> {
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
export async function getSessionAndUserFromSessionId(sessionId: string): Promise<UserAndSession | null> {
  const { session, user: authEntity } = await auth.validateSession(sessionId);

  if (!session || !authEntity) {
    return {
      user: null,
      session: null,
    };
  }

  return {
    session,
    user: await getAuthUserData(authEntity.userId)
  }
}

async function getAuthUserData(userId: {= userEntityUpper =}['id']): Promise<AuthUserData> {
  const user = await prisma.{= userEntityLower =}
    .findUnique({
      where: { id: userId },
      include: {
        {= authFieldOnUserEntityName =}: {
          include: {
            {= identitiesFieldOnAuthEntityName =}: true
          }
        }
      }
    })

  if (!user) {
    throwInvalidCredentialsError()
  }

  return createAuthUserData(user);
}

// PRIVATE API
export function invalidateSession(sessionId: string): Promise<void> {
  return auth.invalidateSession(sessionId);
}
