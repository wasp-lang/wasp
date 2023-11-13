{{={= =}=}}
import { sign, verify } from '../core/auth.js'
import AuthError from '../core/AuthError.js'
import HttpError from '../core/HttpError.js'
import prisma from '../dbClient.js'
import { isPrismaError, prismaErrorToHttpError, sleep } from '../utils.js'
import { type {= userEntityUpper =} } from '../entities/index.js'
import { type Prisma } from '@prisma/client';

import { throwValidationError } from './validation.js'

{=# additionalSignupFields.isDefined =}
{=& additionalSignupFields.importStatement =}
{=/ additionalSignupFields.isDefined =}

{=# additionalSignupFields.isDefined =}
const _waspAdditionalSignupFieldsConfig = {= additionalSignupFields.importIdentifier =}
{=/ additionalSignupFields.isDefined =}
{=^ additionalSignupFields.isDefined =}
import { createDefineAdditionalSignupFieldsFn } from './providers/types.js'
const _waspAdditionalSignupFieldsConfig = {} as ReturnType<
  ReturnType<typeof createDefineAdditionalSignupFieldsFn<never>>
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

export async function findUserBy(where: Prisma.{= userEntityUpper =}WhereUniqueInput): Promise<{= userEntityUpper =}> {
  return prisma.{= userEntityLower =}.findUnique({ where });
}

export async function createUser(data: Prisma.{= userEntityUpper =}CreateInput): Promise<{= userEntityUpper =}> {
  try {
    return await prisma.{= userEntityLower =}.create({ data })
  } catch (e) {
    rethrowPossiblePrismaError(e);
  }
}

export async function deleteUser(user: {= userEntityUpper =}): Promise<{= userEntityUpper =}> {
  try {
    return await prisma.{= userEntityLower =}.delete({ where: { id: user.id } })
  } catch (e) {
    rethrowPossiblePrismaError(e);
  }
}

export async function createAuthToken(user: {= userEntityUpper =}): Promise<string> {
  return sign(user.id);
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
