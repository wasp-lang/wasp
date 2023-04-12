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
import { isValidEmail } from '../core/auth/validators.js'
import { emailSender } from '../email/index.js';
import { Email } from '../email/core/types.js';
{=/ isEmailAuthEnabled =}

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

const EMAIL_FIELD = 'email';
const PASSWORD_FIELD = 'password';
const TOKEN_FIELD = 'token';

const emailValidators = [
  { validates: EMAIL_FIELD, message: 'email must be present', validator: email => !!email },
  { validates: EMAIL_FIELD, message: 'email must be a valid email', validator: email => isValidEmail(email) },
];
const passwordValidators = [
  { validates: PASSWORD_FIELD, message: 'password must be present', validator: password => !!password },
  { validates: PASSWORD_FIELD, message: 'password must be at least 8 characters', validator: password => password.length >= 8 },
  { validates: PASSWORD_FIELD, message: 'password must contain a number', validator: password => /\d/.test(password) },
];
const tokenValidators = [
  { validates: TOKEN_FIELD, message: 'token must be present', validator: token => !!token },
];

export function ensureValidEmailAndPassword(args: unknown): void {
  ensureValidEmail(args);
  ensureValidPassword(args);
}

export function ensureValidTokenAndNewPassword(args: unknown): void {
  validate(args, [
    ...tokenValidators,
  ]);
  ensureValidPassword(args);
}

export function ensureValidEmail(args: unknown): void {
  validate(args, [
    ...emailValidators,
  ]);
}

export function ensureValidPassword(args: unknown): void {
  validate(args, [
    ...passwordValidators,
  ]);
}

function validate(args: unknown, validators: { validates: string, message: string, validator: (value: unknown) => boolean }[]): void {
  for (const { validates, message, validator } of validators) {
    if (!validator(args[validates])) {
      throwValidationError(message);
    }
  }
}
{=/ isEmailAuthEnabled =}

export function throwInvalidCredentialsError(message?: string): void {
  throw new HttpError(401, 'Invalid credentials', { message })
}

function rethrowPossiblePrismaError(e: unknown): void {
  if (e instanceof AuthError) {
    throwValidationError(e.message);
  } else if (isPrismaError(e)) {
    throw prismaErrorToHttpError(e)
  } else {
    throw new HttpError(500)
  }
}

function throwValidationError(message: string): void {
  throw new HttpError(422, 'Validation failed', { message })
}