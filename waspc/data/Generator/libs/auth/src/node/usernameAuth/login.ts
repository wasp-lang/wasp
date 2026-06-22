import type { ProviderIdFor } from "../authService/types";
import { verifyPassword } from "../password";
import { createProviderId, getProviderDataWithPassword } from "../providerData";
import { getUsernameLoginCredentials } from "./credentials";
import { throwInvalidCredentials } from "./errors";
import type { UsernameAuthLoginResult, UsernameLoginArgs } from "./types";

export async function loginWithUsername<RequestContext, User>({
  fields,
  request,
  adapters,
}: UsernameLoginArgs<RequestContext, User>): Promise<UsernameAuthLoginResult> {
  const credentials = getUsernameLoginCredentials(fields);
  const providerId = createProviderId(
    "username",
    credentials.username,
  ) as ProviderIdFor<"username">;
  const authIdentity = await adapters.authRepository.findIdentity(providerId);
  if (!authIdentity) {
    throwInvalidCredentials();
  }

  try {
    const providerData = getProviderDataWithPassword<"username">(
      authIdentity.providerData,
    );
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
