import type { ProviderIdFor } from "../authService/types";
import {
  createProviderId,
  getProviderDataWithPassword,
  sanitizeAndSerializeProviderData,
} from "../providerData";
import { getEmailSignupCredentials } from "./credentials";
import { throwEmailDeliveryFailed, throwEmailResendTooSoon } from "./errors";
import { getEmailResendStatus } from "./resend";
import type { EmailSignupArgs, EmailSignupResult } from "./types";

export async function signupWithEmail<
  RequestContext,
  CreatedUser,
  UserFields extends object = object,
>({
  fields,
  request,
  getUserFields,
  isEmailAutoVerified,
  adapters,
}: EmailSignupArgs<
  RequestContext,
  CreatedUser,
  UserFields
>): Promise<EmailSignupResult> {
  const credentials = getEmailSignupCredentials(fields);
  const providerId = createProviderId(
    "email",
    credentials.email,
  ) as ProviderIdFor<"email">;
  const existingAuthIdentity =
    await adapters.authRepository.findIdentity(providerId);

  if (existingAuthIdentity) {
    const providerData = getProviderDataWithPassword<"email">(
      existingAuthIdentity.providerData,
    );

    if (providerData.isEmailVerified) {
      await adapters.workSimulator.doFakeWork();
      return { success: true };
    }

    const { isResendAllowed, timeLeft } = getEmailResendStatus({
      sentAt: providerData.passwordResetSentAt,
      clock: adapters.clock,
    });
    if (!isResendAllowed) {
      throwEmailResendTooSoon(timeLeft);
    }

    await adapters.authRepository.deleteUserByAuthId(
      existingAuthIdentity.authId,
    );
  }

  const userFields = await getUserFields?.(fields);
  const serializedProviderData =
    await sanitizeAndSerializeProviderData<"email">({
      hashedPassword: credentials.password,
      isEmailVerified: isEmailAutoVerified ? true : false,
      emailVerificationSentAt: null,
      passwordResetSentAt: null,
    });

  await adapters.hooks.onBeforeSignup({ request, providerId });
  const createdUser = await adapters.authRepository.createUserWithIdentity({
    providerId,
    serializedProviderData,
    userFields,
  });
  await adapters.hooks.onAfterSignup({
    request,
    providerId,
    user: createdUser.user,
  });

  if (isEmailAutoVerified) {
    return { success: true };
  }

  const verificationLink =
    await adapters.emailVerification.createVerificationLink(credentials.email);
  try {
    await adapters.emailVerification.sendVerificationEmail({
      email: credentials.email,
      verificationLink,
    });
  } catch (e) {
    throwEmailDeliveryFailed(e);
  }

  return { success: true };
}
