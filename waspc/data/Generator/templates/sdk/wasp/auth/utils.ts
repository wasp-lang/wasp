{{={= =}=}}
import { prisma, HttpError } from '../server/index.js'
import { sleep } from '../server/utils.js'
import {
  type {= userEntityUpper =},
  type {= authEntityUpper =},
  type {= authIdentityEntityUpper =},
} from '../entities/index.js'
import { Prisma } from '@prisma/client';

import { throwValidationError } from './validation.js'
import { ValidationError } from '@wasp.sh/lib-auth'

import { type UserSignupFields, type PossibleUserFields } from './providers/types.js'
import {
  AuthServiceError,
  createProviderId,
  getProviderData,
  getProviderDataWithPassword,
  mergeAndSerializeProviderDataUpdates,
  sanitizeAndSerializeProviderData,
  type EmailProviderData,
  type OAuthProviderData,
  type PossibleProviderData,
  type ProviderId,
  type ProviderName,
  type UsernameProviderData,
} from '@wasp.sh/lib-auth/node'

export {
  createProviderId,
  getProviderData,
  getProviderDataWithPassword,
  sanitizeAndSerializeProviderData,
}

export type {
  EmailProviderData,
  OAuthProviderData,
  PossibleProviderData,
  ProviderId,
  ProviderName,
  UsernameProviderData,
}

// PRIVATE API
export const contextWithUserEntity = {
  entities: {
    {= userEntityUpper =}: prisma.{= userEntityLower =}
  }
}

// PRIVATE API
export const authConfig = {
  failureRedirectPath: "{= failureRedirectPath =}",
  successRedirectPath: "{= successRedirectPath =}",
}

// PUBLIC API
export async function findAuthIdentity(providerId: ProviderId): Promise<{= authIdentityEntityUpper =} | null> {
  return prisma.{= authIdentityEntityLower =}.findUnique({
    where: {
      providerName_providerUserId: providerId,
    }
  });
}

// PUBLIC API
/**
 * Updates the provider data for the given auth identity.
 * 
 * This function performs data sanitization and serialization.
 * Sanitization is done by hashing the password, so this function
 * expects the password received in the `providerDataUpdates`
 * **not to be hashed**.
 */
export async function updateAuthIdentityProviderData<PN extends ProviderName>(
  providerId: ProviderId,
  existingProviderData: PossibleProviderData[PN],
  providerDataUpdates: Partial<PossibleProviderData[PN]>,
): Promise<{= authIdentityEntityUpper =}> {
  const serializedProviderData = await mergeAndSerializeProviderDataUpdates(
    existingProviderData,
    providerDataUpdates,
  );
  return prisma.{= authIdentityEntityLower =}.update({
    where: {
      providerName_providerUserId: providerId,
    },
    data: { providerData: serializedProviderData },
  });
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

// PUBLIC API
export type CreateUserResult = {= userEntityUpper =} & {
  auth: {= authEntityUpper =} | null
}

// PUBLIC API
export async function createUser(
  providerId: ProviderId,
  serializedProviderData?: string,
  userFields?: PossibleUserFields,
): Promise<CreateUserResult> {
  return prisma.{= userEntityLower =}.create({
    data: {
      // Using any here to prevent type errors when userFields are not
      // defined. We want Prisma to throw an error in that case.
      ...(userFields ?? {} as any),
      {= authFieldOnUserEntityName =}: {
        create: {
          {= identitiesFieldOnAuthEntityName =}: {
              create: {
                  providerName: providerId.providerName,
                  providerUserId: providerId.providerUserId,
                  providerData: serializedProviderData,
              },
          },
        }
      },
    },
    // We need to include the Auth entity here because we need `authId`
    // to be able to create a session.
    include: {
      {= authFieldOnUserEntityName =}: true,
    },
  })
}

// PRIVATE API
export async function deleteUserByAuthId(authId: string): Promise<{ count: number }> {
  return prisma.{= userEntityLower =}.deleteMany({ where: { auth: {
    id: authId,
  } } })
}

// PRIVATE API
// If an user exists, we don't want to leak information
// about it. Pretending that we're doing some work
// will make it harder for an attacker to determine
// if a user exists or not.
// NOTE: Attacker measuring time to response can still determine
// if a user exists or not. We'll be able to avoid it when 
// we implement e-mail sending via jobs.
export async function doFakeWork(): Promise<void> {
  const timeToWork = Math.floor(Math.random() * 1000) + 1000;
  await sleep(timeToWork);
}

// PRIVATE API
export function rethrowPossibleAuthError(e: unknown): void {
  // Prisma code P2002 is for unique constraint violations.
  if (e instanceof Prisma.PrismaClientKnownRequestError && e.code === 'P2002') {
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
export function rethrowPossibleAuthServiceError(e: unknown): void {
  if (e instanceof ValidationError) {
    throwValidationError(e.message)
  }

  if (e instanceof AuthServiceError && e.code === 'invalid-credentials') {
    throw createInvalidCredentialsError()
  }

  if (e instanceof AuthServiceError && e.code === 'email-resend-too-soon') {
    throw new HttpError(400, e.message)
  }

  if (e instanceof AuthServiceError && e.code === 'email-delivery-failed') {
    console.error(e.metadata.logMessage ?? e.message, e.metadata.cause ?? e)
    throw new HttpError(500, String(e.metadata.responseMessage ?? e.message))
  }
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
      throwValidationError(e.message)
    }
  }
  return result;
}

// PRIVATE API
export function createInvalidCredentialsError(message?: string): HttpError {
  return new HttpError(401, 'Invalid credentials', { message })
}
