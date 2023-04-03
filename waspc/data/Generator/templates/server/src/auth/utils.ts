{{={= =}=}}
import { sign, verify } from '../core/auth.js'
import AuthError from '../core/AuthError.js'
import HttpError from '../core/HttpError.js'
import prisma from '../dbClient.js'
import { isPrismaError, prismaErrorToHttpError, sleep } from '../utils.js'
import { type {= userEntityUpper =} } from '../entities/index.js'
import waspServerConfig from '../config.js';

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

export async function deleteUser(user: {= userEntityUpper =}): Promise<{= userEntityUpper =}> {
  try {
    return await prisma.{= userEntityLower =}.delete({ where: { id: user.id } })
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

export async function createEmailVerificationLink(user: {= userEntityUpper =}, clientRoute: string): Promise<string> {
  const token = await createEmailVerificationToken(user);
  return `${waspServerConfig.frontendUrl}${clientRoute}?token=${token}`;
}

export async function createPasswordResetLink(user: {= userEntityUpper =}, clientRoute: string): Promise<string> {
  const token = await createPasswordResetToken(user);
  return `${waspServerConfig.frontendUrl}${clientRoute}?token=${token}`;
}

async function createEmailVerificationToken(user: {= userEntityUpper =}): Promise<string> {
  return sign(user.id, { expiresIn: '30m' });
}

async function createPasswordResetToken(user: {= userEntityUpper =}): Promise<string> {
  return sign(user.id, { expiresIn: '30m' });
}
{=/ isEmailAuthEnabled =}
