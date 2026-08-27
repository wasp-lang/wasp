{{={= =}=}}
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
  type PossibleProviderSecrets,
  parseProviderData,
  parseProviderSecrets,
  serializeProviderData,
  serializeProviderSecrets,
} from '../../auth/providerData.js'

import { type UserSignupFields, type PossibleUserFields } from '../../auth/providers/types.js'

// Runtime-agnostic provider data code, re-exported here because it's part of
// the server-side auth API surface (e.g. through `wasp/server/auth`).
export {
  createProviderId,
  normalizeProviderUserId,
  parseProviderData,
  parseProviderSecrets,
  type ProviderId,
  type ProviderName,
  type PossibleProviderData,
  type PossibleProviderSecrets,
  type EmailProviderData,
  type EmailProviderSecrets,
  type UsernameProviderData,
  type UsernameProviderSecrets,
  type OAuthProviderData,
  type OAuthProviderSecrets,
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
/**
 * The auth identity as everything outside auth internals sees it: the secret
 * column does not exist here -- the Prisma client omits it by default, and only
 * `findAuthIdentitySecrets` opts back in.
 */
export type AuthIdentityWithoutSecrets = Omit<{= authIdentityEntityUpper =}, 'providerSecrets'>

// PUBLIC API
export async function findAuthIdentity(providerId: ProviderId): Promise<AuthIdentityWithoutSecrets | null> {
  return prisma.{= authIdentityEntityLower =}.findUnique({
    where: {
      providerName_providerUserId: providerId,
    }
  });
}

{=^ isCustomAuthProviderUsed =}
// PUBLIC API
/**
 * Reads the auth identity's secret material (e.g. the password hash). This is
 * the single place that opts back into the `providerSecrets` column the Prisma
 * client omits by default -- keep the result on the server.
 */
export async function findAuthIdentitySecrets<PN extends ProviderName>(
  providerId: ProviderId,
): Promise<PossibleProviderSecrets[PN] | null> {
  const identity = await prisma.{= authIdentityEntityLower =}.findUnique({
    where: {
      providerName_providerUserId: providerId,
    },
    omit: { providerSecrets: false },
  });
  return identity === null ? null : parseProviderSecrets<PN>(identity.providerSecrets);
}

// PUBLIC API
/**
 * Merges the given updates into the auth identity's non-secret provider data.
 * Secrets have their own column and their own writer (`setAuthIdentitySecrets`),
 * so this update can never touch them.
 */
export async function updateAuthIdentityProviderData<PN extends ProviderName>(
  providerId: ProviderId,
  providerDataUpdates: Partial<PossibleProviderData[PN]>,
): Promise<AuthIdentityWithoutSecrets> {
  const identity = await prisma.{= authIdentityEntityLower =}.findUnique({
    where: {
      providerName_providerUserId: providerId,
    },
    select: { providerData: true },
  });
  if (identity === null) {
    throw new Error('Auth identity not found.');
  }
  const newProviderData = {
    ...parseProviderData<PN>(identity.providerData),
    ...providerDataUpdates,
  }
  return prisma.{= authIdentityEntityLower =}.update({
    where: {
      providerName_providerUserId: providerId,
    },
    data: { providerData: serializeProviderData<PN>(newProviderData) },
  });
}

// PUBLIC API
/**
 * Replaces the auth identity's secret material. Expects secrets to arrive
 * **already hashed** -- hashing is the flow's explicit responsibility (see
 * `hashPassword` in `wasp/server/auth/password`), never an implicit side effect
 * of storage.
 */
export async function setAuthIdentitySecrets<PN extends ProviderName>(
  providerId: ProviderId,
  secrets: PossibleProviderSecrets[PN],
): Promise<AuthIdentityWithoutSecrets> {
  return prisma.{= authIdentityEntityLower =}.update({
    where: {
      providerName_providerUserId: providerId,
    },
    data: { providerSecrets: serializeProviderSecrets<PN>(secrets) },
  });
}
{=/ isCustomAuthProviderUsed =}

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
/**
 * Creates the user with its auth identity in one atomic write. `data` is the
 * provider's non-secret state, `secrets` its secret material -- `secrets` must
 * arrive already hashed (see `setAuthIdentitySecrets`).
 */
export async function createUser<PN extends ProviderName>(
  providerId: ProviderId,
  identity?: {
    data?: PossibleProviderData[PN];
    secrets?: PossibleProviderSecrets[PN];
  },
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
                  providerData: serializeProviderData<PN>(identity?.data ?? ({} as PossibleProviderData[PN])),
                  providerSecrets: serializeProviderSecrets<PN>(identity?.secrets ?? ({} as PossibleProviderSecrets[PN])),
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

// PRIVATE API
export function createInvalidCredentialsError(message?: string): HttpError {
  return new HttpError(401, 'Invalid credentials', { message })
}
