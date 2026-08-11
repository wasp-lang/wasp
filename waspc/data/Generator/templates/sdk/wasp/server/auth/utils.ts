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
  // We do the sanitization here only on updates, and early (before the retry
  // loop) so the retries below never hash the password a second time.
  const sanitizedProviderDataUpdates = await ensurePasswordIsHashed(providerDataUpdates);

  // `providerData` is the shared, mutable state of an auth identity, and several
  // flows (token issuance, send-metadata, email verification, password reset)
  // read-modify-write the same blob. Persisting a whole blob built from a
  // snapshot can silently overwrite a change another flow just committed (e.g.
  // restoring a just-consumed token hash, or the old password hash). We therefore
  // write the full blob with an optimistic compare-and-set: only write when it is
  // unchanged since it was read, and otherwise re-read the authoritative value,
  // re-apply our own updates, and retry (bounded).
  const RETRIES = 5;
  // The caller's snapshot is the optimistic baseline; on a conflict we fall back
  // to a fresh read so a concurrent change is never overwritten.
  let providerData = existingProviderData;
  let serializedProviderData = serializeProviderData<PN>(providerData);

  for (let attempt = 0; attempt < RETRIES; attempt++) {
    const newProviderData: PossibleProviderData[PN] = {
      ...providerData,
      ...sanitizedProviderDataUpdates,
    };
    const newSerializedProviderData = serializeProviderData<PN>(newProviderData);

    const result = await prisma.{= authIdentityEntityLower =}.updateMany({
      where: {
        providerName: providerId.providerName,
        providerUserId: providerId.providerUserId,
        providerData: serializedProviderData,
      },
      data: { providerData: newSerializedProviderData },
    });
    if (result.count === 1) {
      // updateMany doesn't return the row; re-read it so callers get the updated
      // identity.
      const updated = await findAuthIdentity(providerId);
      if (updated === null) {
        throw new HttpError(500, 'Auth identity not found after update');
      }
      return updated;
    }

    // Lost the race: providerData changed after we read it. Re-read the
    // authoritative value and retry, applying our updates on top of it so the
    // concurrent change is preserved.
    const fresh = await findAuthIdentity(providerId);
    if (fresh === null) {
      throw new HttpError(404, 'Auth identity not found');
    }
    providerData = getProviderDataWithPassword<PN>(fresh.providerData);
    serializedProviderData = fresh.providerData;
  }

  throw new HttpError(409, 'Failed to update auth identity provider data');
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
const ONE_TIME_TOKEN_CONSUME_RETRIES = 5;

// PRIVATE API
/**
 * Atomically consumes a one-time token (email verification / password reset).
 *
 * This is an optimistic compare-and-set: the single `updateMany` only writes
 * when the `providerData` value is byte-for-byte unchanged since it was read,
 * so at most one concurrent request can claim a token, and a concurrent consume
 * of the *other* purpose can't be overwritten (which would resurrect a consumed
 * hash). If the write loses the race (the blob changed), we re-read the
 * authoritative value and retry, up to `ONE_TIME_TOKEN_CONSUME_RETRIES`.
 *
 * Tokens are globally unique (minted with a random `jwtId`) and stored only as
 * SHA-256 hashes; `field` selects the correct purpose slot.
 *
 * `updates` are other provider-data changes applied with the consume (e.g.
 * `isEmailVerified: true`, or a new raw password under `hashedPassword`, which
 * is hashed here). The already-stored password hash is left untouched, so it is
 * not re-hashed.
 */
export async function consumeOneTimeToken(
  providerId: ProviderId,
  field: 'outstandingEmailVerificationToken' | 'outstandingPasswordResetToken',
  token: string,
  updates: Partial<PossibleProviderData['email']>,
  invalidTokenMessage: string,
): Promise<{= authIdentityEntityUpper =}> {
  const tokenHash = sha256(token);

  for (let attempt = 0; attempt < ONE_TIME_TOKEN_CONSUME_RETRIES; attempt++) {
    const authIdentity = await findAuthIdentity(providerId);
    if (!authIdentity) {
      throw new HttpError(400, invalidTokenMessage);
    }
    const existingProviderData = getProviderDataWithPassword<'email'>(authIdentity.providerData);

    // Reject immediately if this is no longer the current outstanding token
    // (already consumed, or superseded by a newly issued token).
    if (existingProviderData[field] !== tokenHash) {
      throw new HttpError(400, invalidTokenMessage);
    }

    // Only hash the password coming in via `updates` (if any) to avoid re-hashing
    // the already-stored password hash. Mirrors `updateAuthIdentityProviderData`.
    const hashedUpdates = await ensurePasswordIsHashed(updates);

    const serializedProviderData = serializeProviderData<'email'>({
      ...existingProviderData,
      ...hashedUpdates,
      [field]: null,
    });

    // Compare-and-set on the exact `providerData` value we read above. If any
    // field changed concurrently, this matches 0 rows and we loop to re-read.
    const result = await prisma.{= authIdentityEntityLower =}.updateMany({
      where: {
        providerName: providerId.providerName,
        providerUserId: providerId.providerUserId,
        providerData: authIdentity.providerData,
      },
      data: { providerData: serializedProviderData },
    });
    if (result.count === 1) {
      return authIdentity;
    }
  }

  throw new HttpError(400, invalidTokenMessage);
}
