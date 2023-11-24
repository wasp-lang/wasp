{{={= =}=}}
import { sign } from '../../../core/auth.js'
import { emailSender } from '../../../email/index.js';
import { Email } from '../../../email/core/types.js';
import { rethrowPossiblePrismaError } from '../../utils.js'
import prisma from '../../../dbClient.js'
import waspServerConfig from '../../../config.js';
import { type {= userEntityUpper =}, type {= authEntityUpper =} } from '../../../entities/index.js'

type {= authEntityUpper =}Id = {= authEntityUpper =}['id']
type {= userEntityUpper =}Id = {= userEntityUpper =}['id']

type AuthWithId = {
  id: {= authEntityUpper =}Id,
}

export async function updateAuthEmailVerification(authId: {= authEntityUpper =}Id) {
  try {
    await prisma.{= authEntityLower =}.update({
      where: { id: authId },
      data: { isEmailVerified: true },
    })
  } catch (e) {
    rethrowPossiblePrismaError(e);
  }
}

export async function updateAuthPassword(authId: {= authEntityUpper =}Id, password: string) {
  try {
    await prisma.{= authEntityLower =}.update({
      where: { id: authId },
      data: { password },
    })
  } catch (e) {
    rethrowPossiblePrismaError(e);
  }
}

export async function createEmailVerificationLink(auth: AuthWithId, clientRoute: string) {
  const token = await createEmailVerificationToken(auth);
  return `${waspServerConfig.frontendUrl}${clientRoute}?token=${token}`;
}

export async function createPasswordResetLink(auth: AuthWithId, clientRoute: string)  {
  const token = await createPasswordResetToken(auth);
  return `${waspServerConfig.frontendUrl}${clientRoute}?token=${token}`;
}

async function createEmailVerificationToken(auth: AuthWithId): Promise<string> {
  return sign(auth.id, { expiresIn: '30m' });
}

async function createPasswordResetToken(auth: AuthWithId): Promise<string> {
  return sign(auth.id, { expiresIn: '30m' });
}

export async function sendPasswordResetEmail(
  email: string,
  content: Email,
) {
  return sendEmailAndLogTimestamp(email, content, 'passwordResetSentAt');
}

export async function sendEmailVerificationEmail(
  email: string,
  content: Email,
) {
  return sendEmailAndLogTimestamp(email, content, 'emailVerificationSentAt');
}

async function sendEmailAndLogTimestamp(
  email: string,
  content: Email,
  field: 'emailVerificationSentAt' | 'passwordResetSentAt',
) {
  // Set the timestamp first, and then send the email
  // so the user can't send multiple requests while
  // the email is being sent.
  try {
    await prisma.{= authEntityLower =}.update({
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
  auth: {= authEntityUpper =},
  field: 'emailVerificationSentAt' | 'passwordResetSentAt',
  resendInterval: number = 1000 * 60,
): boolean {
  const sentAt = auth[field];
  if (!sentAt) {
    return true;
  }
  const now = new Date();
  const diff = now.getTime() - sentAt.getTime();
  return diff > resendInterval;
}
