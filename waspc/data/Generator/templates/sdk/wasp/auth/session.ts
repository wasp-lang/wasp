{{={= =}=}}
import { Request as ExpressRequest, Response as ExpressResponse } from "express";

import { type {= userEntityUpper =} } from '../entities/index.js';
import { type AuthUserData } from '../server/auth/user.js';

import { auth } from "./lucia.js";
import type { Session, Cookie } from "lucia";
import { createInvalidCredentialsError } from "./utils.js";

import { prisma } from 'wasp/server';
import { createAuthUserData } from "../server/auth/user.js";

type SessionAndUser = {
  session: Session;
  user: AuthUserData;
}

{=# isCookieAuthEnabled =}
/* Cookie Functions */
// PRIVATE API
export async function createSession(authId: string, res: ExpressResponse): Promise<Session> {
  const session = await auth.createSession(authId, {});
  const sessionCookie = createSessionCookieWithHttpOnly(session.id);
  res.setHeader("Set-Cookie", sessionCookie.serialize());
  return session;
}

// PRIVATE API
export function getSessionFromCookie(cookieHeader: string): string | null {
  return auth.readSessionCookie(cookieHeader);
}

// PRIVATE API
export function createSessionCookieWithHttpOnly(sessionId: string): Cookie {
  const cookie = auth.createSessionCookie(sessionId);
  cookie.attributes.httpOnly = true;
  return cookie;
}

// PRIVATE API
export function createBlankCookie(): Cookie {
  const cookie = auth.createBlankSessionCookie();
  cookie.attributes.httpOnly = true;
  return cookie;
}

// PRIVATE API
export function invalidateSession(sessionId: string, res: ExpressResponse): Promise<void> {
  res.setHeader("Set-Cookie", createBlankCookie().serialize())
  return auth.invalidateSession(sessionId)
}
{=/ isCookieAuthEnabled =}

{=^ isCookieAuthEnabled =}
/* JWT Functions */
// PRIVATE API
export async function createSession(authId: string, res: ExpressResponse): Promise<Session> {
  return auth.createSession(authId, {});
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
export function invalidateSession(sessionId: string, res: ExpressResponse): Promise<void> {
  return auth.invalidateSession(sessionId);
}
{=/ isCookieAuthEnabled =}

/* Universal Functions */
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
