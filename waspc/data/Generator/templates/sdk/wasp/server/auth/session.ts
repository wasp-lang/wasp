{{={= =}=}}
import { type {= userEntityUpper =} } from '../../entities/index.js';
import { type AuthUserData } from '../../auth/user.js';

import { auth } from "./lucia.js";
import type { Session } from "lucia";
import { createInvalidCredentialsError } from "./utils.js";

import { prisma } from '../index.js';
import { createAuthUserData } from "../../auth/user.js";

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
export async function getSessionAndUserFromSessionId(sessionId: string): Promise<SessionAndUser | null> {
  const { session, user: authEntity } = await auth.validateSession(sessionId);

  if (!session || !authEntity) {
    return null;
  }

  // Such a session can't identify a user, so we treat it as unauthenticated.
  if (authEntity.userId === null) {
    return null;
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
    throw createInvalidCredentialsError()
  }

  return createAuthUserData(user);
}

// PRIVATE API
export function invalidateSession(sessionId: string): Promise<void> {
  return auth.invalidateSession(sessionId);
}

// PRIVATE API
// Invalidates all sessions belonging to the `authId` in the database
export function invalidateAllSessionsForAuthId(authId: string): Promise<void> {
  return auth.invalidateUserSessions(authId);
}
