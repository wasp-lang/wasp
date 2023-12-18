{{={= =}=}}
import { sign } from '../../../core/auth.js'
import { emailSender } from '../../../email/index.js';
import { Email } from '../../../email/core/types.js';
import {
  rethrowPossibleAuthError,
  updateAuthIdentityProviderData,
  findAuthIdentity,
  deserializeAndSanitizeProviderData,
} from '../../utils.js';
import waspServerConfig from '../../../config.js';
import { type {= userEntityUpper =}, type {= authEntityUpper =} } from '../../../entities/index.js'

export async function createEmailVerificationLink(providerUserId: string, clientRoute: string) {
  const token = await createEmailVerificationToken(providerUserId);
  return `${waspServerConfig.frontendUrl}${clientRoute}?token=${token}`;
}

export async function createPasswordResetLink(providerUserId: string, clientRoute: string)  {
  const token = await createPasswordResetToken(providerUserId);
  return `${waspServerConfig.frontendUrl}${clientRoute}?token=${token}`;
}

async function createEmailVerificationToken(providerUserId: string): Promise<string> {
  return sign(providerUserId, { expiresIn: '30m' });
}

async function createPasswordResetToken(providerUserId: string): Promise<string> {
  return sign(providerUserId, { expiresIn: '30m' });
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
    const providerData = deserializeAndSanitizeProviderData<'email'>(authIdentity.providerData);
    await updateAuthIdentityProviderData<'email'>('email', email, providerData, {
        [field]: new Date()
    });
  } catch (e) {
    rethrowPossibleAuthError(e);  
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
