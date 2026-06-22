import type { ProviderIdFor } from "../authService/types";
import { createProviderId, getProviderDataWithPassword } from "../providerData";
import { getPasswordResetFields } from "./credentials";
import { throwInvalidToken } from "./errors";
import type { EmailPasswordResetArgs, EmailPasswordResetResult } from "./types";

const invalidPasswordResetTokenMessage = "Password reset failed, invalid token";

export async function resetPassword({
  fields,
  adapters,
}: EmailPasswordResetArgs): Promise<EmailPasswordResetResult> {
  const { token, password } = getPasswordResetFields(fields);
  const { email } = await adapters.tokenService
    .verifyEmailToken(token)
    .catch(() => {
      throwInvalidToken(invalidPasswordResetTokenMessage);
    });

  const providerId = createProviderId("email", email) as ProviderIdFor<"email">;
  const authIdentity = await adapters.authRepository.findIdentity(providerId);
  if (!authIdentity) {
    throwInvalidToken(invalidPasswordResetTokenMessage);
  }

  const providerData = getProviderDataWithPassword<"email">(
    authIdentity.providerData,
  );
  await adapters.authRepository.updateIdentityProviderData({
    providerId,
    existingProviderData: providerData,
    providerDataUpdates: {
      isEmailVerified: true,
      hashedPassword: password,
    },
  });

  return { success: true };
}
