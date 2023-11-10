{{={= =}=}}
import { sign, verify } from '../core/auth.js'
import AuthError from '../core/AuthError.js'
import HttpError from '../core/HttpError.js'
import prisma from '../dbClient.js'
import { isPrismaError, prismaErrorToHttpError, sleep } from '../utils.js'
import { type {= userEntityUpper =} } from '../entities/index.js'
import waspServerConfig from '../config.js';
import { type Prisma } from '@prisma/client';
{=# isEmailAuthEnabled =}
import { emailSender } from '../email/index.js';
import { Email } from '../email/core/types.js';
{=/ isEmailAuthEnabled =}

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

{=# isEmailAuthEnabled =}
export async function updateUserEmailVerification(userId: {= userEntityUpper =}Id): Promise<void> {
  try {
    await prisma.{= userEntityLower =}.update({
      where: { id: userId },
      data: { isEmailVerified: true },
    })
  } catch (e) {
    rethrowPossiblePrismaError(e);
  }
}

export async function updateUserPassword(userId: {= userEntityUpper =}Id, password: string): Promise<void> {
  try {
    await prisma.{= userEntityLower =}.update({
      where: { id: userId },
      data: { password },
    })
  } catch (e) {
    rethrowPossiblePrismaError(e);
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

export async function sendPasswordResetEmail(
  email: string,
  content: Email,
): Promise<void> {
  return sendEmailAndLogTimestamp(email, content, 'passwordResetSentAt');
}

export async function sendEmailVerificationEmail(
  email: string,
  content: Email,
): Promise<void> {
  return sendEmailAndLogTimestamp(email, content, 'emailVerificationSentAt');
}

async function sendEmailAndLogTimestamp(
  email: string,
  content: Email,
  field: 'emailVerificationSentAt' | 'passwordResetSentAt',
): Promise<void> {
  // Set the timestamp first, and then send the email
  // so the user can't send multiple requests while
  // the email is being sent.
  try {
    await prisma.{= userEntityLower =}.update({
      where: { email },
      data: { [field]: new Date() },
    })
  } catch (e) {
    rethrowPossiblePrismaError(e);  
  }
  emailSender.send(content).catch((e) => {
    console.error(`Failed to send email for ${field}`, e);
  });
}

export function isEmailResendAllowed(
  user: {= userEntityUpper =},
  field: 'emailVerificationSentAt' | 'passwordResetSentAt',
  resendInterval: number = 1000 * 60,
): boolean {
  const sentAt = user[field];
  if (!sentAt) {
    return true;
  }
  const now = new Date();
  const diff = now.getTime() - sentAt.getTime();
  return diff > resendInterval;
}
{=/ isEmailAuthEnabled =}

function rethrowPossiblePrismaError(e: unknown): void {
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
