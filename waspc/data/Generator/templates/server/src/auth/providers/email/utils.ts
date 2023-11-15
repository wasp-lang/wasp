{{={= =}=}}
import { sign } from '../../../core/auth.js'
import { emailSender } from '../../../email/index.js';
import { Email } from '../../../email/core/types.js';
import { rethrowPossiblePrismaError } from '../../utils.js'
import prisma from '../../../dbClient.js'
import waspServerConfig from '../../../config.js';
import { type {= userEntityUpper =} } from '../../../entities/index.js'

type {= userEntityUpper =}Id = {= userEntityUpper =}['id']

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
