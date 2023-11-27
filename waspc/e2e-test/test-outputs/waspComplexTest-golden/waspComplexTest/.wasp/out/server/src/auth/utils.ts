import { sign, verify } from '../core/auth.js'
import AuthError from '../core/AuthError.js'
import HttpError from '../core/HttpError.js'
import prisma from '../dbClient.js'
import { isPrismaError, prismaErrorToHttpError, sleep } from '../utils.js'
import { type User, type Auth } from '../entities/index.js'
import { type Prisma } from '@prisma/client';

import { throwValidationError } from './validation.js'


import { defineAdditionalSignupFields, type PossibleAdditionalSignupFields } from './providers/types.js'
const _waspAdditionalSignupFieldsConfig = {} as ReturnType<typeof defineAdditionalSignupFields>

export const contextWithUserEntity = {
  entities: {
    User: prisma.user
  }
}

export const authConfig = {
  failureRedirectPath: "/login",
  successRedirectPath: "/",
}

export async function findAuthWithUserBy(where: Prisma.AuthWhereInput) {
  return prisma.auth.findFirst({ where, include: { user: true }});
}

export async function createAuthWithUser(data: Prisma.AuthCreateInput, additionalFields?: PossibleAdditionalSignupFields) {
  try {
    return await prisma.auth.create({
      data: {
        ...data,
        user: {
          create: {
            // Using any here to prevent type errors when additionalFields are not
            // defined. We want Prisma to throw an error in that case.
            ...(additionalFields ?? {} as any),
          }
        }
      }
    })
  } catch (e) {
    rethrowPossiblePrismaError(e);
  }
}

export async function deleteAuth(auth: Auth) {
  try {
    return await prisma.auth.delete({ where: { id: auth.id } })
  } catch (e) {
    rethrowPossiblePrismaError(e);
  }
}

export async function createAuthToken(
  auth: Auth & { user: User }
): Promise<string> {
  return sign(auth.user.id);
}

export async function verifyToken(token: string): Promise<{ id: any }> {
  return verify(token);
}

// If an user exists, we don't want to leak information
// about it. Pretending that we're doing some work
// will make it harder for an attacker to determine
// if a user exists or not.
// NOTE: Attacker measuring time to response can still determine
// if a user exists or not. We'll be able to avoid it when 
// we implement e-mail sending via jobs.
export async function doFakeWork() {
  const timeToWork = Math.floor(Math.random() * 1000) + 1000;
  return sleep(timeToWork);
}

export function rethrowPossiblePrismaError(e: unknown): void {
  if (e instanceof AuthError) {
    throwValidationError(e.message);
  } else if (isPrismaError(e)) {
    throw prismaErrorToHttpError(e)
  } else {
    throw new HttpError(500)
  }
}

export async function validateAndGetAdditionalFields(data: {
  [key: string]: unknown
}) {
  const {
    password: _password,
    ...sanitizedData
  } = data;
  const result: Record<string, any> = {};
  for (const [field, getFieldValue] of Object.entries(_waspAdditionalSignupFieldsConfig)) {
    try {
      const value = await getFieldValue(sanitizedData)
      result[field] = value
    } catch (e) {
      throwValidationError(e.message)
    }
  }
  return result;
}
