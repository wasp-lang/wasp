{{={= =}=}}
import { sign } from '../../../core/auth.js'
import { emailSender } from '../../../email/index.js';
import { Email } from '../../../email/core/types.js';
import {
  rethrowPossibleAuthError,
  createProviderId,
  updateAuthIdentityProviderData,
  findAuthIdentity,
  deserializeAndSanitizeProviderData,
} from '../../utils.js';
import waspServerConfig from '../../../config.js';
import { type {= userEntityUpper =}, type {= authEntityUpper =} } from '../../../entities/index.js'

export async function createEmailVerificationLink(email: string, clientRoute: string) {
  const token = await createEmailVerificationToken(email);
  return `${waspServerConfig.frontendUrl}${clientRoute}?token=${token}`;
}

export async function createPasswordResetLink(email: string, clientRoute: string)  {
  const token = await createPasswordResetToken(email);
  return `${waspServerConfig.frontendUrl}${clientRoute}?token=${token}`;
}

async function createEmailVerificationToken(email: string): Promise<string> {
  return sign(email, { expiresIn: '30m' });
}

async function createPasswordResetToken(email: string): Promise<string> {
  return sign(email, { expiresIn: '30m' });
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
    const providerId = createProviderId("email", email);
    const authIdentity = await findAuthIdentity(providerId);
    const providerData = deserializeAndSanitizeProviderData<'email'>(authIdentity.providerData);
    await updateAuthIdentityProviderData<'email'>(providerId, providerData, {
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
