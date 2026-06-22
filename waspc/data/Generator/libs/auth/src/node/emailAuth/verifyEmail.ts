import type { ProviderIdFor } from "../authService/types";
import { createProviderId, getProviderDataWithPassword } from "../providerData";
import { getEmailVerificationFields } from "./credentials";
import { throwInvalidToken } from "./errors";
import type { EmailVerificationArgs, EmailVerificationResult } from "./types";

const invalidEmailVerificationTokenMessage =
  "Email verification failed, invalid token";

export async function verifyEmail<RequestContext, User>({
  fields,
  request,
  adapters,
}: EmailVerificationArgs<
  RequestContext,
  User
>): Promise<EmailVerificationResult> {
  const { token } = getEmailVerificationFields(fields);
  const { email } = await adapters.tokenService
    .verifyEmailToken(token)
    .catch(() => {
      throwInvalidToken(invalidEmailVerificationTokenMessage);
    });

  const providerId = createProviderId("email", email) as ProviderIdFor<"email">;
  const authIdentity = await adapters.authRepository.findIdentity(providerId);
  if (!authIdentity) {
    throwInvalidToken(invalidEmailVerificationTokenMessage);
  }

  const providerData = getProviderDataWithPassword<"email">(
    authIdentity.providerData,
  );
  await adapters.authRepository.updateIdentityProviderData({
    providerId,
    existingProviderData: providerData,
    providerDataUpdates: { isEmailVerified: true },
  });

  const auth = await adapters.authRepository.findAuthWithUserByAuthId(
    authIdentity.authId,
  );
  if (!auth) {
    throwInvalidToken(invalidEmailVerificationTokenMessage);
  }

  await adapters.hooks.onAfterEmailVerified({
    request,
    email,
    user: auth.user,
  });

  return { success: true };
}
