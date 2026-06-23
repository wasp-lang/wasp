import type { ProviderIdFor } from "../authService/types";
import { createProviderId, getProviderDataWithPassword } from "../providerData";
import { getPasswordResetRequestFields } from "./credentials";
import { throwEmailDeliveryFailed, throwEmailResendTooSoon } from "./errors";
import { getPasswordResetResendStatus } from "./resend";
import type {
  EmailPasswordResetRequestArgs,
  EmailPasswordResetRequestResult,
} from "./types";

export async function requestPasswordReset({
  fields,
  adapters,
}: EmailPasswordResetRequestArgs): Promise<EmailPasswordResetRequestResult> {
  const { email } = getPasswordResetRequestFields(fields);
  const providerId = createProviderId("email", email) as ProviderIdFor<"email">;
  const authIdentity = await adapters.authRepository.findIdentity(providerId);

  if (!authIdentity) {
    await adapters.workSimulator.doFakeWork();
    return { success: true };
  }

  const providerData = getProviderDataWithPassword<"email">(
    authIdentity.providerData,
  );
  const { isResendAllowed, timeLeft } = getPasswordResetResendStatus({
    providerData,
    clock: adapters.clock,
  });
  if (!isResendAllowed) {
    throwEmailResendTooSoon(timeLeft);
  }

  const passwordResetLink =
    await adapters.passwordReset.createPasswordResetLink(email);
  try {
    await adapters.passwordReset.sendPasswordResetEmail({
      email: authIdentity.providerUserId,
      passwordResetLink,
    });
  } catch (e) {
    throwEmailDeliveryFailed(e, {
      logMessage: "Failed to send password reset email:",
      responseMessage: "Failed to send password reset email.",
    });
  }

  return { success: true };
}
