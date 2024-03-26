{{={= =}=}}
import { hashPassword } from 'wasp/auth/password'
import { prisma, HttpError } from 'wasp/server'
import { sleep } from 'wasp/server/utils'
import {
  type {= userEntityUpper =},
  type {= authEntityUpper =},
  type {= authIdentityEntityUpper =},
} from 'wasp/entities/index'
import { Prisma } from '@prisma/client';

import { throwValidationError } from 'wasp/auth/validation'

import { type UserSignupFields, type PossibleUserFields } from 'wasp/auth/providers/types'

export type EmailProviderData = {
  hashedPassword: string;
  isEmailVerified: boolean;
  emailVerificationSentAt: string | null;
  passwordResetSentAt: string | null;
}

export type UsernameProviderData = {
  hashedPassword: string;
}

export type OAuthProviderData = {}

/**
 * This type is used for type-level programming e.g. to enumerate
 * all possible provider data types.
 * 
 * The keys of this type are the names of the providers and the values
 * are the types of the provider data.
 */
export type PossibleProviderData = {
  email: EmailProviderData;
  username: UsernameProviderData;
  google: OAuthProviderData;
  keycloak: OAuthProviderData;
  github: OAuthProviderData;
}

export type ProviderName = keyof PossibleProviderData

export const contextWithUserEntity = {
  entities: {
    {= userEntityUpper =}: prisma.{= userEntityLower =}
  }
}

export const authConfig = {
  failureRedirectPath: "{= failureRedirectPath =}",
  successRedirectPath: "{= successRedirectPath =}",
}

/**
 * ProviderId uniquely identifies an auth identity e.g. 
 * "email" provider with user id "test@test.com" or
 * "google" provider with user id "1234567890".
 * 
 * We use this type to avoid passing the providerName and providerUserId
 * separately. Also, we can normalize the providerUserId to make sure it's
 * consistent across different DB operations.
 */
export type ProviderId = {
  providerName: ProviderName;
  providerUserId: string;
}

export function createProviderId(providerName: ProviderName, providerUserId: string): ProviderId {
  return {
    providerName,
    providerUserId: providerUserId.toLowerCase(),
  }
}

export async function findAuthIdentity(providerId: ProviderId): Promise<{= authIdentityEntityUpper =} | null> {
  return prisma.{= authIdentityEntityLower =}.findUnique({
    where: {
      providerName_providerUserId: providerId,
    }
  });
}

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
  // We are doing the sanitization here only on updates to avoid
  // hashing the password multiple times.
  const sanitizedProviderDataUpdates = await sanitizeProviderData(providerDataUpdates);
  const newProviderData = {
    ...existingProviderData,
    ...sanitizedProviderDataUpdates,
  }
  const serializedProviderData = await serializeProviderData<PN>(newProviderData);
  return prisma.{= authIdentityEntityLower =}.update({
    where: {
      providerName_providerUserId: providerId,
    },
    data: { providerData: serializedProviderData },
  });
}

type FindAuthWithUserResult = {= authEntityUpper =} & {
  {= userFieldOnAuthEntityName =}: {= userEntityUpper =}
}

export async function findAuthWithUserBy(
  where: Prisma.{= authEntityUpper =}WhereInput
): Promise<FindAuthWithUserResult> {
  return prisma.{= authEntityLower =}.findFirst({ where, include: { {= userFieldOnAuthEntityName =}: true }});
}

export async function createUser(
  providerId: ProviderId,
  serializedProviderData?: string,
  userFields?: PossibleUserFields,
): Promise<{= userEntityUpper =} & {
  auth: {= authEntityUpper =}
}> {
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

export async function deleteUserByAuthId(authId: string): Promise<{ count: number }> {
  return prisma.{= userEntityLower =}.deleteMany({ where: { auth: {
    id: authId,
  } } })
}

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

export function deserializeAndSanitizeProviderData<PN extends ProviderName>(
  providerData: string,
  { shouldRemovePasswordField = false }: { shouldRemovePasswordField?: boolean } = {},
): PossibleProviderData[PN] {
  // NOTE: We are letting JSON.parse throw an error if the providerData is not valid JSON.
  let data = JSON.parse(providerData) as PossibleProviderData[PN];

  if (providerDataHasPasswordField(data) && shouldRemovePasswordField) {
    delete data.hashedPassword;
  }

  return data;
}

export async function sanitizeAndSerializeProviderData<PN extends ProviderName>(
  providerData: PossibleProviderData[PN],
): Promise<string> {
  return serializeProviderData(
    await sanitizeProviderData(providerData)
  );
}

function serializeProviderData<PN extends ProviderName>(providerData: PossibleProviderData[PN]): string {
  return JSON.stringify(providerData);
}

async function sanitizeProviderData<PN extends ProviderName>(
  providerData: PossibleProviderData[PN],
): Promise<PossibleProviderData[PN]> {
  const data = {
    ...providerData,
  };
  if (providerDataHasPasswordField(data)) {
    data.hashedPassword = await hashPassword(data.hashedPassword);
  }

  return data;
}


function providerDataHasPasswordField(
  providerData: PossibleProviderData[keyof PossibleProviderData],
): providerData is { hashedPassword: string } {
  return 'hashedPassword' in providerData;
}

export function throwInvalidCredentialsError(message?: string): void {
  throw new HttpError(401, 'Invalid credentials', { message })
}
