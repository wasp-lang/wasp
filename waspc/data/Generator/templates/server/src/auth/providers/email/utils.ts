{{={= =}=}}
import { sign } from '../../../core/auth.js'
import { emailSender } from '../../../email/index.js';
import { Email } from '../../../email/core/types.js';
import {
  rethrowPossiblePrismaError,
  updateAuthIdentityProviderData,
  findAuthIdentity,
  deserializeProviderData,
} from '../../utils.js';
import waspServerConfig from '../../../config.js';
import { type {= userEntityUpper =}, type {= authEntityUpper =} } from '../../../entities/index.js'

type {= authEntityUpper =}Id = {= authEntityUpper =}['id']
type {= userEntityUpper =}Id = {= userEntityUpper =}['id']

type AuthWithId = {
  id: {= authEntityUpper =}Id,
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
    const authIdentity = await findAuthIdentity("email", email);
    const providerData = deserializeProviderData<'email'>(authIdentity.providerData);
    await updateAuthIdentityProviderData<'email'>(authIdentity.authId, providerData, {
        [field]: new Date()
    });
  } catch (e) {
    rethrowPossiblePrismaError(e);  
  }
  emailSender.send(content).catch((e) => {
    console.error(`Failed to send email for ${field}`, e);
  });
}

export function isEmailResendAllowed<Field extends 'emailVerificationSentAt' | 'passwordResetSentAt'>(
  fields: {
    [field in Field]: Date | null
  },
  field: Field,
  resendInterval: number = 1000 * 60,
): boolean {
  const sentAt = fields[field];
  if (!sentAt) {
    return true;
  }
  const now = new Date();
  const diff = now.getTime() - new Date(sentAt).getTime();
  return diff > resendInterval;
}
