{{={= =}=}}
import { sign, verify } from '../core/auth.js'
import AuthError from '../core/AuthError.js'
import HttpError from '../core/HttpError.js'
import prisma from '../dbClient.js'
import { isPrismaError, prismaErrorToHttpError, sleep } from '../utils.js'
import { type {= userEntityUpper =} } from '../entities/index.js'
import { type Prisma, type {= authEntityUpper =} } from '@prisma/client';

import { throwValidationError } from './validation.js'

{=# additionalSignupFields.isDefined =}
{=& additionalSignupFields.importStatement =}
{=/ additionalSignupFields.isDefined =}

import { createDefineAdditionalSignupFieldsFn, type PossibleAdditionalSignupFields } from './providers/types.js'
{=# additionalSignupFields.isDefined =}
const _waspAdditionalSignupFieldsConfig = {= additionalSignupFields.importIdentifier =}
{=/ additionalSignupFields.isDefined =}
{=^ additionalSignupFields.isDefined =}
const _waspAdditionalSignupFieldsConfig = {} as ReturnType<
  ReturnType<typeof createDefineAdditionalSignupFieldsFn>
>
{=/ additionalSignupFields.isDefined =}

export const contextWithUserEntity = {
  entities: {
    {= userEntityUpper =}: prisma.{= userEntityLower =}
  }
}

export const authConfig = {
  failureRedirectPath: "{= failureRedirectPath =}",
  successRedirectPath: "{= successRedirectPath =}",
}

export async function findAuthWithUserBy(where: Prisma.{= authEntityUpper =}WhereInput) {
  return prisma.{= authEntityLower =}.findFirst({ where, include: { {= userFieldOnAuthEntityName =}: true }});
}

export async function createAuthWithUser(data: Prisma.{= authEntityUpper =}CreateInput, additionalFields: PossibleAdditionalSignupFields) {
  try {
    return await prisma.{= authEntityLower =}.create({
      data: {
        ...data,
        {= userFieldOnAuthEntityName =}: {
          create: {
            ...additionalFields,
          }
        }
      }
    })
  } catch (e) {
    rethrowPossiblePrismaError(e);
  }
}

export async function deleteUser(auth: {= authEntityUpper =}) {
  try {
    return await prisma.{= authEntityLower =}.delete({ where: { id: auth.id } })
  } catch (e) {
    rethrowPossiblePrismaError(e);
  }
}

export async function createAuthToken(
  auth: {= authEntityUpper =} & { {= userFieldOnAuthEntityName =}: {= userEntityUpper =} }
): Promise<string> {
  return sign(auth.{= userFieldOnAuthEntityName =}.id);
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
