import { namespaceFor } from "../namespaces.js";
import type { EmailContent, WaspAuthRuntime } from "../types.js";
import { TimeSpan, makeJwt } from "../utils.js";

/**
 * The email method's link and mail helpers. Bound to the runtime the adapter
 * was created with (see `bindEmailHelpers`), so the SDK can re-export them
 * as the `wasp/server/auth/email` public API.
 */
export type EmailHelpers = ReturnType<typeof makeEmailHelpers>;

export function makeEmailHelpers(runtime: WaspAuthRuntime) {
  const { createJWT } = makeJwt(runtime);

  async function createEmailJWT(email: string): Promise<string> {
    return createJWT({ email }, { expiresIn: new TimeSpan(30, "m") });
  }

  async function createEmailVerificationLink(
    email: string,
    clientRoute: string,
  ): Promise<string> {
    return `${runtime.clientUrl}${clientRoute}?token=${await createEmailJWT(email)}`;
  }

  async function createPasswordResetLink(
    email: string,
    clientRoute: string,
  ): Promise<string> {
    return `${runtime.clientUrl}${clientRoute}?token=${await createEmailJWT(email)}`;
  }

  async function sendEmailAndSaveMetadata(
    email: string,
    content: {
      from?: { name?: string; email: string };
      to: string;
    } & EmailContent,
    metadata: Record<string, string | null>,
  ): Promise<void> {
    // Save the metadata (e.g. timestamp) first, and then send the email so
    // the user can't send multiple requests while the email is being sent.
    const emailIdentities = runtime.identityNamespaces(namespaceFor("email"));
    const identity = await emailIdentities.find(email);
    if (!identity) {
      throw new Error(`User with email: ${email} not found.`);
    }
    await emailIdentities.updateData(email, metadata);

    // The `email-send` grant: present by construction, the email method
    // requires app.emailSender.
    if (runtime.email === undefined) {
      throw new Error("The email auth method requires the email-send grant.");
    }
    runtime.email.send(content).catch((e) => {
      console.error("Failed to send email", e);
    });
  }

  function sendPasswordResetEmail(
    email: string,
    content: {
      from?: { name?: string; email: string };
      to: string;
    } & EmailContent,
  ) {
    return sendEmailAndSaveMetadata(email, content, {
      passwordResetSentAt: new Date().toISOString(),
    });
  }

  function sendEmailVerificationEmail(
    email: string,
    content: {
      from?: { name?: string; email: string };
      to: string;
    } & EmailContent,
  ) {
    return sendEmailAndSaveMetadata(email, content, {
      emailVerificationSentAt: new Date().toISOString(),
    });
  }

  return {
    createEmailVerificationLink,
    createPasswordResetLink,
    sendPasswordResetEmail,
    sendEmailVerificationEmail,
  };
}

export function isEmailResendAllowed<
  Field extends "emailVerificationSentAt" | "passwordResetSentAt",
>(
  fields: { [field in Field]?: unknown },
  field: Field,
  resendInterval: number = 1000 * 60,
): { isResendAllowed: boolean; timeLeft: number } {
  const sentAt = fields[field];
  if (typeof sentAt !== "string" || !sentAt) {
    return { isResendAllowed: true, timeLeft: 0 };
  }
  const diff = Date.now() - new Date(sentAt).getTime();
  const isResendAllowed = diff > resendInterval;
  const timeLeft = isResendAllowed
    ? 0
    : Math.round((resendInterval - diff) / 1000);
  return { isResendAllowed, timeLeft };
}
