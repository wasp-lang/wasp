{{={= =}=}}
import { prisma, HttpError } from '../index.js'
import { sleep } from '../utils.js'
import {
  type {= userEntityUpper =},
  type {= authEntityUpper =},
} from '../../entities/index.js'
import { Prisma } from '@prisma/client';

import { type UserSignupFields } from '../../auth/providers/types.js'

// Runtime-agnostic provider data code, re-exported here because it's part of
// the server-side auth API surface (e.g. through `wasp/server/auth`).
export {
  createProviderId,
  parseProviderData,
  parseProviderSecrets,
  type ProviderId,
  type ProviderName,
} from '../../auth/providerData.js'

// PRIVATE API
export const contextWithUserEntity = {
  entities: {
    {= userEntityUpper =}: prisma.{= userEntityLower =}
  }
}

// PRIVATE API
export const authConfig = {
  failureRedirectPath: "{= failureRedirectPath =}",
}

// PRIVATE API
export type FindAuthWithUserResult = {= authEntityUpper =} & {
  {= userFieldOnAuthEntityName =}: {= userEntityUpper =}
}

// PRIVATE API
export async function findAuthWithUserBy(
  where: Prisma.{= authEntityUpper =}WhereInput
): Promise<FindAuthWithUserResult | null> {
  const result = await prisma.{= authEntityLower =}.findFirst({ where, include: { {= userFieldOnAuthEntityName =}: true }});

  if (result === null) {
    return null;
  }

  if (result.user === null) {
    return null;
  }

  return { ...result, user: result.user };
}

// PRIVATE API
// If an user exists, we don't want to leak information
// about it. Pretending that we're doing some work
// will make it harder for an attacker to determine
// if a user exists or not.
// NOTE: Attacker measuring time to response can still determine
// if a user exists or not. We'll be able to avoid it when
// we implement e-mail sending via jobs.
export async function doFakeWork(): Promise<unknown> {
  const timeToWork = Math.floor(Math.random() * 1000) + 1000;
  return sleep(timeToWork);
}

// PRIVATE API
export function rethrowPossibleAuthError(e: unknown): void {
  // Prisma code P2002 is for unique constraint violations.
  if (e instanceof Prisma.PrismaClientKnownRequestError && e.code === 'P2002') {
    throw new HttpError(422, 'Save failed', {
      message: `user with the same identity already exists`,
    })
  }

  // The identity facet reports the same conflict with a contract error code
  // (codes, not classes -- adapter packages hold their own contract copy).
  if (
    typeof e === 'object' && e !== null && 'code' in e &&
    (e as { code: unknown }).code === 'wasp-auth/duplicate-identity'
  ) {
    throw new HttpError(422, 'Save failed', {
      message: `user with the same identity already exists`,
    })
  }

  if (e instanceof Prisma.PrismaClientValidationError) {
    // NOTE: Logging the error since this usually means that there are
    // required fields missing in the request, we want the developer
    // to know about it.
    console.error(e)
    throw new HttpError(422, 'Save failed', {
      message: 'there was a database error'
    })
  }

  // Prisma code P2021 is for missing table errors.
  if (e instanceof Prisma.PrismaClientKnownRequestError && e.code === 'P2021') {
    // NOTE: Logging the error since this usually means that the database
    // migrations weren't run, we want the developer to know about it.
    console.error(e)
    console.info('🐝 This error can happen if you did\'t run the database migrations.')
    throw new HttpError(500, 'Save failed', {
      message: `there was a database error`,
    })
  }

  // Prisma code P2003 is for foreign key constraint failure
  if (e instanceof Prisma.PrismaClientKnownRequestError && e.code === 'P2003') {
    console.error(e)
    console.info(`🐝 This error can happen if you have some relation on your {= userEntityUpper =} entity
   but you didn't specify the "onDelete" behaviour to either "Cascade" or "SetNull".
   Read more at: https://www.prisma.io/docs/orm/prisma-schema/data-model/relations/referential-actions`)
    throw new HttpError(500, 'Save failed', {
      message: `there was a database error`,
    })
  }

  throw e
}

// PRIVATE API
export async function validateAndGetUserFields(
  data: {
    [key: string]: unknown
  },
  userSignupFields?: UserSignupFields,
): Promise<Record<string, any>> {
  const {
    password: _password,
    ...sanitizedData
  } = data;
  const result: Record<string, any> = {};

  if (!userSignupFields) {
    return result;
  }

  for (const [field, getFieldValue] of Object.entries(userSignupFields)) {
    try {
      const value = await getFieldValue(sanitizedData)
      result[field] = value
    } catch (e) {
      throw new HttpError(422, 'Validation failed', { message: e.message })
    }
  }
  return result;
}

// PRIVATE API
export function createInvalidCredentialsError(message?: string): HttpError {
  return new HttpError(401, 'Invalid credentials', { message })
}
