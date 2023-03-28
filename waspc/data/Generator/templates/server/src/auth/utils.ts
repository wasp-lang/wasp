{{={= =}=}}
import { sign, verify } from '../core/auth.js'
import AuthError from '../core/AuthError.js'
import HttpError from '../core/HttpError.js'
import prisma from '../dbClient.js'
import { isPrismaError, prismaErrorToHttpError } from '../utils.js'
import { type {= userEntityUpper =} } from '../entities/index.js'

type {= userEntityUpper =}Id = {= userEntityUpper =}['id']

export const contextWithUserEntity = {
  entities: {
    {= userEntityUpper =}: prisma.{= userEntityLower =}
  }
}

export const authConfig = {
  failureRedirectPath: "{= failureRedirectPath =}",
  successRedirectPath: "{= successRedirectPath =}",
}

export async function findUserBy<K extends keyof {= userEntityUpper =}>(where: { [key in K]: {= userEntityUpper =}[K] }): Promise<{= userEntityUpper =}> {
  return prisma.{= userEntityLower =}.findUnique({ where });
}

export async function createUser(data: {= userEntityUpper =}): Promise<{= userEntityUpper =}> {
  try {
    return await prisma.{= userEntityLower =}.create({ data })
  } catch (e) {
    if (e instanceof AuthError) {
      throw new HttpError(422, 'Validation failed', { message: e.message })
    } else if (isPrismaError(e)) {
      throw prismaErrorToHttpError(e)
    } else {
      throw new HttpError(500)
    }
  }
}

export async function createAuthToken(user: {= userEntityUpper =}): Promise<string> {
  return sign(user.id);
}

export async function verifyToken(token: string): Promise<{ id: any }> {
  return verify(token);
}
{=# isEmailAuthEnabled =}
export async function updateUserEmailVerification(userId: {= userEntityUpper =}Id): Promise<void> {
  try {
    await prisma.{= userEntityLower =}.update({
      where: { id: userId },
      data: { isEmailVerified: true },
    })
  } catch (e) {
    if (e instanceof AuthError) {
      throw new HttpError(422, 'Validation failed', { message: e.message })
    } else if (isPrismaError(e)) {
      throw prismaErrorToHttpError(e)
    } else {
      throw new HttpError(500)
    }
  }
}

export async function updateUserPassword(userId: {= userEntityUpper =}Id, password: string): Promise<void> {
  try {
    await prisma.{= userEntityLower =}.update({
      where: { id: userId },
      data: { password },
    })
  } catch (e) {
    if (e instanceof AuthError) {
      throw new HttpError(422, 'Validation failed', { message: e.message })
    } else if (isPrismaError(e)) {
      throw prismaErrorToHttpError(e)
    } else {
      throw new HttpError(500)
    }
  }
}
{=/ isEmailAuthEnabled =}

export async function createEmailVerificationToken(user: {= userEntityUpper =}): Promise<string> {
  return sign(user.id);
}

export async function createPasswordResetToken(user: {= userEntityUpper =}): Promise<string> {
  return sign(user.id);
}
