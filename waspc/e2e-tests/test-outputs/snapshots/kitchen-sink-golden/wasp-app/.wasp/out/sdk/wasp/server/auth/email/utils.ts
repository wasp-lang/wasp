import { isEmailResendAllowed } from '@wasp.sh/lib-sdk-core'
// PUBLIC API
export { isEmailResendAllowed } from '@wasp.sh/lib-sdk-core'

import { createJWT, TimeSpan } from '../jwt.js'
import { emailSender } from '../../email/index.js';
import type { Email } from '../../email/core/types.js';
import {
  createProviderId,
  updateAuthIdentityProviderData,
  findAuthIdentity,
  getProviderDataWithPassword,
  type EmailProviderData,
} from '../utils.js';
import { config as waspServerConfig } from '../../index.js';
import type { User, Auth } from '../../../entities/index.js'

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
