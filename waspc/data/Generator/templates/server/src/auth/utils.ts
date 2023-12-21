{{={= =}=}}
import { hashPassword, sign, verify } from '../core/auth.js'
import AuthError from '../core/AuthError.js'
import HttpError from '../core/HttpError.js'
import prisma from '../dbClient.js'
import { sleep } from '../utils.js'
import {
  type {= userEntityUpper =},
  type {= authEntityUpper =},
  type {= authIdentityEntityUpper =},
} from '../entities/index.js'
import { Prisma } from '@prisma/client';

import { PASSWORD_FIELD, throwValidationError } from './validation.js'

{=# additionalSignupFields.isDefined =}
{=& additionalSignupFields.importStatement =}
{=/ additionalSignupFields.isDefined =}

import { defineAdditionalSignupFields, type PossibleAdditionalSignupFields } from './providers/types.js'
{=# additionalSignupFields.isDefined =}
const _waspAdditionalSignupFieldsConfig = {= additionalSignupFields.importIdentifier =}
{=/ additionalSignupFields.isDefined =}
{=^ additionalSignupFields.isDefined =}
const _waspAdditionalSignupFieldsConfig = {} as ReturnType<typeof defineAdditionalSignupFields>
{=/ additionalSignupFields.isDefined =}

export type EmailProviderData = {
  hashedPassword: string;
  isEmailVerified: boolean;
  emailVerificationSentAt: Date | null;
  passwordResetSentAt: Date | null;
}

export type UsernameProviderData = {
  hashedPassword: string;
}

export type OAuthProviderData = {}

export type PossibleProviderData = {
  email: EmailProviderData;
  username: UsernameProviderData;
  google: OAuthProviderData;
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

type ProviderUserId = {
  id: string;
}

export function createProviderUserId(providerUserId: string): ProviderUserId {
  return {
    id: providerUserId.toLowerCase(),
  }
}

export async function findAuthIdentity(providerName: ProviderName, providerUserId: ProviderUserId): Promise<{= authIdentityEntityUpper =} | null> {
  return prisma.{= authIdentityEntityLower =}.findUnique({
    where: {
      providerName_providerUserId: {
        providerName,
        providerUserId: providerUserId.id,
      }
    }
  });
}

export async function updateAuthIdentityProviderData<PN extends ProviderName>(
  providerName: ProviderName,
  providerUserId: ProviderUserId,
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
      providerName_providerUserId: {
        providerName,
        providerUserId: providerUserId.id,
      }
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
  providerName: ProviderName,
  providerUserId: ProviderUserId,
  serializedProviderData?: string,
  userFields?: PossibleAdditionalSignupFields,
): Promise<{= userEntityUpper =}> {
  try {
    return prisma.{= userEntityLower =}.create({
      data: {
        // Using any here to prevent type errors when userFields are not
        // defined. We want Prisma to throw an error in that case.
        ...(userFields ?? {} as any),
        {= authFieldOnUserEntityName =}: {
          create: {
            {= identitiesFieldOnAuthEntityName =}: {
                create: {
                    providerName,
                    providerUserId: providerUserId.id,
                    providerData: serializedProviderData,
                },
            },
          }
        },
      }
    })
  } catch (e) {
    rethrowPossibleAuthError(e);
  }
}

export async function deleteUserByAuthId(authId: string): Promise<{ count: number }> {
  try {
    return await prisma.{= userEntityLower =}.deleteMany({ where: { auth: {
      id: authId,
    } } })
  } catch (e) {
    rethrowPossibleAuthError(e);
  }
}

export async function createAuthToken(
  userId: {= userEntityUpper =}['id']
): Promise<string> {
  return sign(userId);
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
export async function doFakeWork(): Promise<unknown> {
  const timeToWork = Math.floor(Math.random() * 1000) + 1000;
  return sleep(timeToWork);
}

export function rethrowPossibleAuthError(e: unknown): void {
  if (e instanceof AuthError) {
    throwValidationError(e.message);
  }
  
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
  
  // NOTE: Giving extra info to the developer since we don't send
  // the error message to the client.
  console.error(e)
  throw new HttpError(500)
}

export async function validateAndGetAdditionalFields(data: {
  [key: string]: unknown
}): Promise<Record<string, any>> {
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

export function deserializeAndSanitizeProviderData<PN extends ProviderName>(
  providerData: string,
  { shouldRemovePasswordField = false }: { shouldRemovePasswordField?: boolean } = {},
): PossibleProviderData[PN] {
  // NOTE: We are letting JSON.parse throw an error if the providerData is not valid JSON.
  let data = JSON.parse(providerData) as PossibleProviderData[PN];

  if (providerDataHasPasswordField(data) && shouldRemovePasswordField) {
    delete data[PASSWORD_FIELD];
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
    data[PASSWORD_FIELD] = await hashPassword(data[PASSWORD_FIELD]);
  }

  return data;
}


function providerDataHasPasswordField(
  providerData: PossibleProviderData[keyof PossibleProviderData],
): providerData is { hashedPassword: string } {
  return PASSWORD_FIELD in providerData;
}
