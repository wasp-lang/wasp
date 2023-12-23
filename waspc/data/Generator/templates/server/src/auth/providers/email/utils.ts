{{={= =}=}}
import { signData } from '../../../core/auth.js'
import { emailSender } from '../../../email/index.js';
import { Email } from '../../../email/core/types.js';
import {
  createProviderId,
  updateAuthIdentityProviderData,
  findAuthIdentity,
  deserializeAndSanitizeProviderData,
  type EmailProviderData,
} from '../../utils.js';
import { getCurrentUTCDate, getRandomString } from '../../../utils.js'
import waspServerConfig from '../../../config.js';
import { type {= userEntityUpper =}, type {= authEntityUpper =} } from '../../../entities/index.js'

export async function createEmailVerificationLinkWithToken(
  email: string,
  clientRoute: string,
): Promise<{ link: string; token: string; }> {
  const { jwtToken, token } = await createEmailJwtToken(email);
  const link = `${waspServerConfig.frontendUrl}${clientRoute}?token=${jwtToken}`;
  return { link, token };
}

export async function createPasswordResetLinkWithToken(
  email: string,
  clientRoute: string,
): Promise<{ link: string; token: string; }>  {
  const { jwtToken, token } = await createEmailJwtToken(email);
  const link = `${waspServerConfig.frontendUrl}${clientRoute}?token=${jwtToken}`;
  return { link, token };
}

async function createEmailJwtToken(email: string): Promise<{ jwtToken: string; token: string; }> {
  const token = getRandomString();
  const jwtToken = await signData({ email, token }, { expiresIn: '30m' });
  return { jwtToken, token };
}

export async function sendPasswordResetEmail(
  email: string,
  passwordResetToken: string,
  content: Email,
): Promise<void> {
  return sendEmailAndSaveMetadata(email, content, {
    passwordResetSentAt: getCurrentUTCDate().toISOString(),
    passwordResetToken,
  });
}

export async function sendEmailVerificationEmail(
  email: string,
  emailVerificationToken: string,
  content: Email,
): Promise<void> {
  return sendEmailAndSaveMetadata(email, content, {
    emailVerificationSentAt: getCurrentUTCDate().toISOString(),
    emailVerificationToken,
  });
}

async function sendEmailAndSaveMetadata(
  email: string,
  content: Email,
  metadata: Partial<EmailProviderData>,
): Promise<void> {
  // Save the metadata (e.g. timestamp) first, and then send the email
  // so the user can't send multiple requests while the email is being sent.
  const providerId = createProviderId("email", email);
  const authIdentity = await findAuthIdentity(providerId);
  const providerData = deserializeAndSanitizeProviderData<'email'>(authIdentity.providerData);
  await updateAuthIdentityProviderData<'email'>(providerId, providerData, metadata);

  emailSender.send(content).catch((e) => {
    console.error('Failed to send email', e);
  });
}

export function isEmailResendAllowed<Field extends 'emailVerificationSentAt' | 'passwordResetSentAt'>(
  fields: {
    [field in Field]: string | null
  },
  field: Field,
  resendInterval: number = 1000 * 60,
): boolean {
  const sentAt = fields[field];
  if (!sentAt) {
    return true;
  }
  const now = getCurrentUTCDate();
  const diff = now.getTime() - new Date(sentAt).getTime();
  return diff > resendInterval;
}
