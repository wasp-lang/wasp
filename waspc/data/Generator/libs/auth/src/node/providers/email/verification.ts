import type { EmailFromField } from "@wasp.sh/lib-sdk-core/node";
import type { AuthEntityTypes } from "../../../shared/entities.js";
import {
  createProviderId,
  getProviderDataWithPassword,
  type EmailProviderData,
} from "../../../shared/providerData.js";
import type { AuthIdentities } from "../../identities.js";
import { TimeSpan, type JwtHelpers } from "../../jwt.js";

/** The least an email the auth flows send needs. The app's `Email` type extends it. */
export type AuthEmail = {
  from?: EmailFromField;
  to: string;
  subject: string;
  text: string;
  html: string;
};

export type EmailVerificationConfig<
  T extends AuthEntityTypes,
  Email extends AuthEmail,
> = {
  frontendUrl: string;
  jwt: JwtHelpers;
  emailSender: { send: (email: Email) => Promise<unknown> };
  identities: Pick<
    AuthIdentities<T>,
    "findAuthIdentity" | "updateAuthIdentityProviderData"
  >;
};

export type EmailVerification<Email extends AuthEmail> = {
  createEmailVerificationLink: (
    email: string,
    clientRoute: string,
  ) => Promise<string>;
  createPasswordResetLink: (
    email: string,
    clientRoute: string,
  ) => Promise<string>;
  sendPasswordResetEmail: (email: string, content: Email) => Promise<void>;
  sendEmailVerificationEmail: (email: string, content: Email) => Promise<void>;
};

/**
 * Creates the link builders and email senders of the email verification and
 * password reset flows.
 */
export function createEmailVerification<
  T extends AuthEntityTypes,
  Email extends AuthEmail,
>(config: EmailVerificationConfig<T, Email>): EmailVerification<Email> {
  const { frontendUrl, jwt, emailSender, identities } = config;

  async function createEmailVerificationLink(
    email: string,
    clientRoute: string,
  ): Promise<string> {
    const { jwtToken } = await createEmailJWT(email);
    return `${frontendUrl}${clientRoute}?token=${jwtToken}`;
  }

  async function createPasswordResetLink(
    email: string,
    clientRoute: string,
  ): Promise<string> {
    const { jwtToken } = await createEmailJWT(email);
    return `${frontendUrl}${clientRoute}?token=${jwtToken}`;
  }

  async function createEmailJWT(email: string): Promise<{ jwtToken: string }> {
    const jwtToken = await jwt.createJWT(
      { email },
      { expiresIn: new TimeSpan(30, "m") },
    );
    return { jwtToken };
  }

  async function sendPasswordResetEmail(
    email: string,
    content: Email,
  ): Promise<void> {
    return sendEmailAndSaveMetadata(email, content, {
      passwordResetSentAt: new Date().toISOString(),
    });
  }

  async function sendEmailVerificationEmail(
    email: string,
    content: Email,
  ): Promise<void> {
    return sendEmailAndSaveMetadata(email, content, {
      emailVerificationSentAt: new Date().toISOString(),
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
    const authIdentity = await identities.findAuthIdentity(providerId);

    if (!authIdentity) {
      throw new Error(`User with email: ${email} not found.`);
    }

    const providerData = getProviderDataWithPassword<"email">(
      authIdentity.providerData,
    );
    await identities.updateAuthIdentityProviderData<"email">(
      providerId,
      providerData,
      metadata,
    );

    emailSender.send(content).catch((e) => {
      console.error("Failed to send email", e);
    });
  }

  return {
    createEmailVerificationLink,
    createPasswordResetLink,
    sendPasswordResetEmail,
    sendEmailVerificationEmail,
  };
}
