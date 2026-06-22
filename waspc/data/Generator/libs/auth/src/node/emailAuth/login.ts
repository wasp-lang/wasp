import type { ProviderIdFor } from "../authService/types";
import { verifyPassword } from "../password";
import { createProviderId, getProviderDataWithPassword } from "../providerData";
import { getEmailLoginCredentials } from "./credentials";
import { throwInvalidCredentials } from "./errors";
import type { EmailAuthLoginResult, EmailLoginArgs } from "./types";

export async function loginWithEmail<RequestContext, User>({
  fields,
  request,
  adapters,
}: EmailLoginArgs<RequestContext, User>): Promise<EmailAuthLoginResult> {
  const credentials = getEmailLoginCredentials(fields);
  const providerId = createProviderId(
    "email",
    credentials.email,
  ) as ProviderIdFor<"email">;
  const authIdentity = await adapters.authRepository.findIdentity(providerId);
  if (!authIdentity) {
    throwInvalidCredentials();
  }

  const providerData = getProviderDataWithPassword<"email">(
    authIdentity.providerData,
  );
  if (!providerData.isEmailVerified) {
    throwInvalidCredentials();
  }

  try {
    await verifyPassword(providerData.hashedPassword, credentials.password);
  } catch (_e) {
    throwInvalidCredentials();
  }

  const auth = await adapters.authRepository.findAuthWithUserByAuthId(
    authIdentity.authId,
  );
  if (auth === null) {
    throwInvalidCredentials();
  }

  await adapters.hooks.onBeforeLogin({
    request,
    providerId,
    user: auth.user,
  });
  const session = await adapters.sessionService.createSession(auth.authId);
  await adapters.hooks.onAfterLogin({
    request,
    providerId,
    user: auth.user,
  });

  return { sessionId: session.id };
}
