{{={= =}=}}
import { createHash } from "node:crypto";
import { hashPassword } from './password.js'
import { prisma, HttpError } from '../index.js'
import { sleep } from '../utils.js'
import {
  type {= userEntityUpper =},
  type {= authEntityUpper =},
  type {= authIdentityEntityUpper =},
} from '../../entities/index.js'
import { Prisma } from '@prisma/client';

import { throwValidationError } from '../../auth/validation.js'

import {
  type ProviderId,
  type ProviderName,
  type PossibleProviderData,
  providerDataHasPasswordField,
} from '../../auth/providerData.js'

import { type UserSignupFields, type PossibleUserFields } from '../../auth/providers/types.js'

// Runtime-agnostic provider data code, re-exported here because it's part of
// the server-side auth API surface (e.g. through `wasp/server/auth`).
export {
  createProviderId,
  normalizeProviderUserId,
  getProviderData,
  getProviderDataWithPassword,
  type ProviderId,
  type ProviderName,
  type PossibleProviderData,
  type EmailProviderData,
  type UsernameProviderData,
  type OAuthProviderData,
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
  // We are doing the sanitization here only on updates to avoid
  // hashing the password multiple times.
  const sanitizedProviderDataUpdates = await ensurePasswordIsHashed(providerDataUpdates);
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
      throwValidationError(e.message)
    }
  }
  return result;
}

// PUBLIC API
export async function sanitizeAndSerializeProviderData<PN extends ProviderName>(
  providerData: PossibleProviderData[PN],
): Promise<string> {
  return serializeProviderData(
    await ensurePasswordIsHashed(providerData)
  );
}

function serializeProviderData<PN extends ProviderName>(providerData: PossibleProviderData[PN]): string {
  return JSON.stringify(providerData);
}

async function ensurePasswordIsHashed<PN extends ProviderName>(
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

// PRIVATE API
export function createInvalidCredentialsError(message?: string): HttpError {
  return new HttpError(401, 'Invalid credentials', { message })
}

// PRIVATE API
// One-time tokens (e.g. email verification, password reset) are stored in the
// user's provider data only as SHA-256 hashes, so a leaked DB doesn't directly
// reveal usable tokens. We compare an incoming token against its hash.
export function sha256(value: string): string {
  return createHash('sha256').update(value).digest('hex');
}

// PRIVATE API
/**
 * Atomically consumes a one-time token (email verification / password reset).
 *
 * The outstanding-token check is embedded in the WHERE clause of a single
 * UPDATE statement (matching the JSON `providerData` fragment
 * `"<field>":"<sha256(token)>"`), so at most one concurrent request can claim
 * the token: once the winning request clears the field, any racing request's
 * WHERE no longer matches and is rejected (count === 0). Each token is globally
 * unique (minted with a random `jwtId`), and `field` selects the correct
 * purpose slot.
 *
 * `updates` are other provider-data changes applied atomically with the consume
 * (e.g. `isEmailVerified: true`, or a new raw password under `hashedPassword`,
 * which is hashed here). The already-stored password hash is left untouched, so
 * it is not re-hashed.
 */
export async function consumeOneTimeToken(
  providerId: ProviderId,
  field: 'outstandingEmailVerificationToken' | 'outstandingPasswordResetToken',
  token: string,
  updates: Partial<PossibleProviderData['email']>,
  invalidTokenMessage: string,
): Promise<{= authIdentityEntityUpper =}> {
  const authIdentity = await findAuthIdentity(providerId);
  if (!authIdentity) {
    throw new HttpError(400, invalidTokenMessage);
  }
  const existingProviderData = getProviderDataWithPassword<'email'>(authIdentity.providerData);

  // Only hash the password coming in via `updates` (if any) to avoid re-hashing
  // the already-stored password hash. Mirrors `updateAuthIdentityProviderData`.
  const hashedUpdates = await ensurePasswordIsHashed(updates);

  const newProviderData = {
    ...existingProviderData,
    ...hashedUpdates,
    [field]: null,
  };
  const serializedProviderData = serializeProviderData<'email'>(newProviderData);

  // Compare-and-set: only update if the stored data still holds this exact
  // outstanding token hash, and reject (count === 0) if it was already consumed
  // or replaced by a newer token.
  const result = await prisma.{= authIdentityEntityLower =}.updateMany({
    where: {
      providerName_providerUserId: providerId,
      providerData: { contains: `"${field}":"${sha256(token)}"` },
    },
    data: { providerData: serializedProviderData },
  });
  if (result.count === 0) {
    throw new HttpError(400, invalidTokenMessage);
  }

  return authIdentity;
}
