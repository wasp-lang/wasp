{{={= =}=}}
import { Request as ExpressRequest } from "express";

import { type {= userEntityUpper =} } from "../entities/index.js"
import { type SanitizedUser } from '../_types/index.js'

import { auth } from "./lucia.js";
import {
  throwInvalidCredentialsError,
  deserializeAndSanitizeProviderData,
} from "./utils.js";

import prisma from '../dbClient.js';

// Lucia's Session is tied to the Auth model,
// so we keep the Auth ID in the session.
export async function createSession(authId: string) {
  return auth.createSession(authId, {});
}

export function getSessionAndUserFromBearerToken(req: ExpressRequest) {
  const authorizationHeader = req.headers["authorization"];

  if (typeof authorizationHeader !== "string") {
    return {
      user: null,
      session: null,
    };
  }

  const sessionId = auth.readBearerToken(authorizationHeader ?? "");
  if (!sessionId) {
    return {
      user: null,
      session: null,
    };
  }

  return getSessionAndUserFromSessionId(sessionId);
}

export async function getSessionAndUserFromSessionId(sessionId: string) {
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

async function getUser(userId: {= userEntityUpper =}['id']): Promise<SanitizedUser> {
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

  // TODO: This logic must match the type in _types/index.ts (if we remove the
  // password field from the object here, we must to do the same there).
  // Ideally, these two things would live in the same place:
  // https://github.com/wasp-lang/wasp/issues/965
  const deserializedIdentities = user.{= authFieldOnUserEntityName =}.{= identitiesFieldOnAuthEntityName =}.map((identity) => {
    const deserializedProviderData = deserializeAndSanitizeProviderData(
      identity.providerData,
      {
        shouldRemovePasswordField: true,
      }
    )
    return {
      ...identity,
      providerData: deserializedProviderData,
    }
  })
  return {
    ...user,
    auth: {
      ...user.auth,
      identities: deserializedIdentities,
    },
  }
}

export function invalidateSession(sessionId: string) {
  return auth.invalidateSession(sessionId);
}
