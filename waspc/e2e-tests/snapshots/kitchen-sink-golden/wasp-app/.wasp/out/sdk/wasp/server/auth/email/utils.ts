import { createJWT, TimeSpan } from 'wasp/auth/jwt'
import { emailSender } from 'wasp/server/email';
import { Email } from 'wasp/server/email/core/types';
import {
  createProviderId,
  updateAuthIdentityProviderData,
  findAuthIdentity,
  getProviderDataWithPassword,
  type EmailProviderData,
} from 'wasp/auth/utils';
import { config as waspServerConfig } from 'wasp/server';
import { type User, type Auth } from 'wasp/entities'

// PUBLIC API
export async function createEmailVerificationLink(
  email: string,
  clientRoute: string,
): Promise<string> {
  const { jwtToken } = await createEmailJWT(email);
  return `${waspServerConfig.frontendUrl}${clientRoute}?token=${jwtToken}`;
}

// PUBLIC API
export async function createPasswordResetLink(
  email: string,
  clientRoute: string,
): Promise<string>  {
  const { jwtToken } = await createEmailJWT(email);
  return `${waspServerConfig.frontendUrl}${clientRoute}?token=${jwtToken}`;
}

async function createEmailJWT(email: string): Promise<{ jwtToken: string; }> {
  const jwtToken = await createJWT({ email }, { expiresIn: new TimeSpan(30, "m") });
  return { jwtToken };
}

// PUBLIC API
export async function sendPasswordResetEmail(
  email: string,
  content: Email,
): Promise<void> {
  return sendEmailAndSaveMetadata(email, content, {
    passwordResetSentAt: (new Date()).toISOString(),
  });
}

// PUBLIC API
export async function sendEmailVerificationEmail(
  email: string,
  content: Email,
): Promise<void> {
  return sendEmailAndSaveMetadata(email, content, {
    emailVerificationSentAt: (new Date()).toISOString(),
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

  if (!authIdentity) {
    throw new Error(`User with email: ${email} not found.`);
  }

  const providerData = getProviderDataWithPassword<'email'>(authIdentity.providerData);
  await updateAuthIdentityProviderData<'email'>(providerId, providerData, metadata);

  emailSender.send(content).catch((e) => {
    console.error('Failed to send email', e);
  });
}

// PUBLIC API
export function isEmailResendAllowed<Field extends 'emailVerificationSentAt' | 'passwordResetSentAt'>(
  fields: {
    [field in Field]: string | null
  },
  field: Field,
  resendInterval: number = 1000 * 60,
): {
  isResendAllowed: boolean;
  timeLeft: number;
} {
  const sentAt = fields[field];
  if (!sentAt) {
    return {
      isResendAllowed: true,
      timeLeft: 0,
    };
  }
  const now = new Date();
  const diff = now.getTime() - new Date(sentAt).getTime();
  const isResendAllowed = diff > resendInterval;
  // Time left in seconds
  const timeLeft = isResendAllowed ? 0 : Math.round((resendInterval - diff) / 1000);
  return { isResendAllowed, timeLeft };
}
